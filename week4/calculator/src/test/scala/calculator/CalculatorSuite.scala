package calculator

import calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, _}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.junit.JUnitRunner

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers with ScalaFutures {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("self ref detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("a"))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("simple 2x loop detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("a"))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("simple 4x loop detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("c")),
      "c" -> Signal(Ref("d")),
      "d" -> Signal(Ref("a"))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("complex self ref detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Ref("a"), Literal(1)))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("complex 2x loop detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Ref("b"), Literal(1))),
      "b" -> Signal(Plus(Ref("a"), Literal(1)))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("complex 4x loop detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Ref("b"), Literal(1))),
      "b" -> Signal(Plus(Ref("c"), Ref("d"))),
      "c" -> Signal(Plus(Ref("d"), Literal(1))),
      "d" -> Signal(Plus(Ref("a"), Literal(1)))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

  test("literal evaluation") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Literal(1))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue === 1)
  }

  test("transitive literal evaluation") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Literal(1))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue === 1)
  }

  test("3x transitive literal evaluation") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Ref("b")),
      "b" -> Signal(Ref("c")),
      "c" -> Signal(Ref("d")),
      "d" -> Signal(Literal(1))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue === 1)
  }

  test("literal + literal evaluation") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Literal(1), Literal(2)))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue === 3)
  }

  test("literal + ref evaluation") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Literal(1), Ref("b"))),
      "b" -> Signal(Literal(2))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue === 3)
  }

  test("ref unknown detection") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Literal(1), Ref("b")))
    )

    val futureTest = Future {
      val namedValues: Map[String, Signal[Double]] =
        Calculator.computeValues(namedExpressions)
      val aSignal: Signal[Double] = namedValues("a")
      namedValues("a")()
    }

    assert(futureTest.futureValue.isNaN)
  }

}
