package workflows4s.wio

/** Workflow linter that detects potential issues in workflow definitions.
  *
  * The linter performs static analysis to identify:
  *   - Busy loops: Loops without timers or signals that could cause infinite execution
  *
  * Note: Clashing events detection is complex and requires runtime information about event types. This is left for future enhancement.
  */
object Linter {

  /** Result of linting a workflow */
  sealed trait LintResult {
    def isValid: Boolean
    def warnings: List[LintWarning]
  }

  object LintResult {
    case class Valid(warnings: List[LintWarning] = Nil) extends LintResult {
      override def isValid: Boolean = true
    }
  }

  /** Linting warning that indicates a potential issue */
  sealed trait LintWarning {
    def message: String
  }

  object LintWarning {
    case class BusyLoop(loopName: Option[String], path: String) extends LintWarning {
      override def message: String = {
        val name = loopName.map(n => s" '$n'").getOrElse("")
        s"Loop$name at path '$path' does not contain any timers or signals. This may cause busy-waiting and excessive resource usage."
      }
    }
  }

  /** Lint a workflow definition
    *
    * @param wio
    *   The workflow to lint
    * @return
    *   Linting result with warnings
    */
  def lint(wio: WIO[?, ?, ?, ?]): LintResult = {
    // We need to cast to work around Scala 3's type system limitations
    val visitor = new LintVisitor[WorkflowContext]()
    visitor.visit(wio.asInstanceOf[WIO[Any, Any, WCState[WorkflowContext], WorkflowContext]])
    LintResult.Valid(visitor.getWarnings())
  }

  private class LintVisitor[Ctx <: WorkflowContext]() {
    private var warnings: List[LintWarning] = Nil
    private var currentPath: List[String]   = Nil

    def getWarnings(): List[LintWarning] = warnings

    def visit[In, Err, Out <: WCState[Ctx]](wio: WIO[In, Err, Out, Ctx]): Unit = wio match {
      case _: WIO.HandleSignal[?, ?, ?, ?, ?, ?, ?] =>
      // Signal handler - no recursion needed

      case WIO.RunIO(_, _, _) =>
      // RunIO - no recursion needed

      case _: WIO.Timer[?, ?, ?, ?] =>
      // Timer - no recursion needed

      case _: WIO.AwaitingTime[?, ?, ?, ?] =>
      // AwaitingTime - no recursion needed

      case WIO.Checkpoint(base, _, _) =>
        visit(base)

      case _: WIO.Recovery[?, ?, ?, ?, ?] =>
      // Recovery - no recursion needed

      case WIO.FlatMap(base, _, _) =>
        visit(base)
      // Can't visit getNext without executing it

      case WIO.AndThen(first, second) =>
        visit(first)
        visit(second)

      case WIO.HandleError(base, _, _, _) =>
        withPath("base") {
          visit(base)
        }
      // handleError is a function, can't visit without executing

      case WIO.HandleErrorWith(base, handleError, _, _) =>
        withPath("base") {
          visit(base)
        }
        withPath("errorHandler") {
          visit(handleError)
        }

      case loop @ WIO.Loop(body, _, onRestart, current, meta, _) =>
        val loopPath = currentPath.mkString(" -> ")

        // Check for busy loop
        if !containsTimerOrSignal(body) && !containsTimerOrSignal(onRestart) then {
          warnings = LintWarning.BusyLoop(meta.releaseBranchName.orElse(meta.restartBranchName), loopPath) :: warnings
        }

        withPath("loop:body") {
          visit(body)
        }
        withPath("loop:restart") {
          visit(onRestart)
        }
        current match {
          case WIO.Loop.State.Forward(wio)  => visit(wio)
          case WIO.Loop.State.Backward(wio) => visit(wio)
          case WIO.Loop.State.Finished(wio) => visit(wio)
        }

      case WIO.Fork(branches, name, _) =>
        branches.zipWithIndex.foreach { case (branch, idx) =>
          val branchName = branch.name.getOrElse(s"branch$idx")
          withPath(s"fork:$branchName") {
            visit(branch.wio)
          }
        }

      case WIO.Parallel(elements, _, _) =>
        elements.zipWithIndex.foreach { case (elem, idx) =>
          withPath(s"parallel:$idx") {
            visit(elem.wio)
          }
        }

      case WIO.HandleInterruption(base, interruption, _, _) =>
        withPath("base") {
          visit(base)
        }
        withPath("interruption") {
          visit(interruption)
        }

      case WIO.Retry(base, _) =>
        visit(base)

      case _: WIO.Pure[?, ?, ?, ?]               =>
      case _: WIO.End[?]                         =>
      case _: WIO.Transform[?, ?, ?, ?, ?, ?, ?] =>
      case _: WIO.Executed[?, ?, ?, ?]           =>
      case _: WIO.Discarded[?, ?]                =>
      case _                                     =>
      // Catch-all for ForEach, Embedded, and any other cases we can't handle
    }

    private def withPath[T](pathSegment: String)(f: => T): T = {
      currentPath = currentPath :+ pathSegment
      try f
      finally currentPath = currentPath.dropRight(1)
    }

    private def containsTimerOrSignal[In, Err, Out <: WCState[Ctx]](wio: WIO[In, Err, Out, Ctx]): Boolean = wio match {
      case _: WIO.HandleSignal[?, ?, ?, ?, ?, ?, ?] => true
      case _: WIO.Timer[?, ?, ?, ?]                 => true
      case _: WIO.AwaitingTime[?, ?, ?, ?]          => true
      case WIO.FlatMap(base, _, _)                  => containsTimerOrSignal(base)
      case WIO.AndThen(first, second)               => containsTimerOrSignal(first) || containsTimerOrSignal(second)
      case WIO.HandleError(base, _, _, _)           => containsTimerOrSignal(base)
      case WIO.HandleErrorWith(base, handler, _, _) => containsTimerOrSignal(base) || containsTimerOrSignal(handler)
      case WIO.Loop(body, _, onRestart, _, _, _)    => containsTimerOrSignal(body) || containsTimerOrSignal(onRestart)
      case WIO.Fork(branches, _, _)                 => branches.exists(b => containsTimerOrSignal(b.wio))
      case WIO.Parallel(elements, _, _)             => elements.exists(e => containsTimerOrSignal(e.wio))
      case WIO.HandleInterruption(base, _, _, _)    => containsTimerOrSignal(base)
      case WIO.Retry(base, _)                       => containsTimerOrSignal(base)
      case WIO.Checkpoint(base, _, _)               => containsTimerOrSignal(base)
      case _                                        => false
    }
  }
}
