package calculator

import calculator.TweetLength.MaxTweetLength
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, _}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

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



  test("Calculator simple test Literals") {
    val names = "abcdefghijk".toList.map(_.toString)

    val namedExpressions: Map[String, Signal[Expr]] = (names  zip (1 to names.size)).map{
      case (c, i) => (c, Signal(Literal(i).asInstanceOf[Expr]))}.toMap

    val expectedResult: Map[String, Double] = (names zip (1.0 to(names.size.toDouble, 1.0))).toMap
    val res = Calculator.computeValues(namedExpressions)

    expectedResult === res.map(a => (a._1, a._2()))
  }

  test("Calculator simple test Refs") {
    val namedExpressions: Map[String, Signal[Expr]] = Map("a" -> Signal(Literal(65.0)), "b" -> Signal(Ref("a")))

    val expectedResult: Map[String, Double] = Map("a" -> 65.0, "b" -> 65)
    val res = Calculator.computeValues(namedExpressions)

    expectedResult === res.map(a => (a._1, a._2()))
  }

  test("Calculator test Refs") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
                                      "a" -> Signal(Literal(65.0)),
                                      "b" -> Signal(Literal(4.0)),
                                      "c" -> Signal(Plus(Ref("a"), Ref("b"))),
                                      "d" -> Signal(Plus(Ref("a"), Literal(1.0))),
                                      "e" -> Signal(Ref("d"))
    )

    val expectedResult: Map[String, Double] = Map("a" -> 65.0, "b" -> 4.0, "c" -> 69.0, "d" -> 66.0, "e" -> 66.0)
    val res = Calculator.computeValues(namedExpressions)

    expectedResult === res.map(a => (a._1, a._2()))
  }

  test("Not existing Ref") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Ref("c"), Literal(1.0))),
      "b" -> Signal(Times(Literal(2.0), Ref("g")))
    )

    val expectedResult: Map[String, Double] = Map("a" -> Double.NaN, "b" -> Double.NaN)
    val res = Calculator.computeValues(namedExpressions)

    expectedResult === res.map(a => (a._1, a._2()))
  }

  test("Cyclic dependency") {
    val namedExpressions: Map[String, Signal[Expr]] = Map(
      "a" -> Signal(Plus(Ref("b"), Literal(1.0))),
      "b" -> Signal(Times(Literal(2.0), Ref("a")))
    )

    val expectedResult: Map[String, Double] = Map("a" -> Double.NaN, "b" -> Double.NaN)
    val res = Calculator.computeValues(namedExpressions)

    expectedResult === res.map(a => (a._1, a._2()))
  }
}
