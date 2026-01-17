package workflows4s.wio

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import workflows4s.wio.Linter.LintWarning
import workflows4s.testing.TestUtils

class LinterTest extends AnyFreeSpec with Matchers {

  "Linter" - {
    "should detect busy loops" - {
      "in a simple loop without timer or signal" in {
        val (_, step) = TestUtils.pure

        val loop = TestCtx2.WIO
          .repeat(step)
          .until(_ => true)
          .onRestartContinue
          .done

        val result = Linter.lint(loop)
        result.isValid.shouldBe(true)
        result.warnings.should(have.size(1))
        result.warnings.head.shouldBe(a[LintWarning.BusyLoop])
      }

      "should not warn when loop body contains a timer" in {
        val (_, timerStep) = TestUtils.timer(1000)

        val loop = TestCtx2.WIO
          .repeat(timerStep)
          .until(_ => true)
          .onRestartContinue
          .done

        val result = Linter.lint(loop)
        result.isValid.shouldBe(true)
        result.warnings.shouldBe(empty)
      }

      "should not warn when loop body contains a signal" in {
        val (_, _, signalStep) = TestUtils.signal

        val loop = TestCtx2.WIO
          .repeat(signalStep)
          .until(_ => true)
          .onRestartContinue
          .done

        val result = Linter.lint(loop)
        result.isValid.shouldBe(true)
        result.warnings.shouldBe(empty)
      }

      "should not warn when restart contains a timer" in {
        val (_, step)         = TestUtils.pure
        val (_, timerRestart) = TestUtils.timer(1000)

        val loop = TestCtx2.WIO
          .repeat(step)
          .until(_ => true)
          .onRestart(timerRestart)
          .done

        val result = Linter.lint(loop)
        result.isValid.shouldBe(true)
        result.warnings.shouldBe(empty)
      }
    }

    "should handle complex workflows" - {
      "with nested structures" in {
        val (_, step1) = TestUtils.pure
        val (_, step2) = TestUtils.timer(1000)

        val workflow = (step1 >>> step2).handleErrorWith(
          TestUtils.errorHandler,
        )

        val result = Linter.lint(workflow)
        result.isValid.shouldBe(true)
        result.warnings.shouldBe(empty)
      }
    }
  }
}
