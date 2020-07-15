import java.io.ByteArrayInputStream

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SecretSantaImprovedSpec extends AnyFlatSpec with Matchers {

  "createListOfNames" should "create a list of Strings of size specified" in {
    val size = 5
    val names = "a\nb\nc\nd\ne"
    val input = s"$size\n$names"
    val is = new ByteArrayInputStream(input.getBytes)

    Console.withIn(is) {
      val result = SecretSantaImproved.createListOfNames
      result should be (List("a", "b", "c", "d", "e"))
      result.size should be (size)
    }
  }

  it should "throw EOFException for no input" in {
    val is = new ByteArrayInputStream("".getBytes)
    Console.withIn(is) {
      a [java.io.EOFException] should be thrownBy SecretSantaImproved.createListOfNames
    }
  }

  it should "throw NumberFormatException for any non-integers" in {
    val is1 = new ByteArrayInputStream("a".getBytes)
    Console.withIn(is1) {
      a [NumberFormatException] should be thrownBy SecretSantaImproved.createListOfNames
    }

    val is2 = new ByteArrayInputStream("1.1".getBytes)
    Console.withIn(is2) {
      a [NumberFormatException] should be thrownBy SecretSantaImproved.createListOfNames
    }
  }

  it should "throw IllegalArgumentException for input size 0 or negative numbers" in {
    val is1 = new ByteArrayInputStream("0".getBytes)
    Console.withIn(is1) {
      an [IllegalArgumentException] should be thrownBy SecretSantaImproved.createListOfNames
    }

    val is2 = new ByteArrayInputStream("-1".getBytes)
    Console.withIn(is2) {
      an [IllegalArgumentException] should be thrownBy SecretSantaImproved.createListOfNames
    }
  }

  "getNames" should "return the input string" in {
    val input = "Kenny"
    val is = new ByteArrayInputStream(input.getBytes)

    Console.withIn(is) {
      val result = SecretSantaImproved.getNames
      result should be (input)
    }
  }

  it should "repeatedly ask for input for empty input until something is submitted" in {
    val name = "123"
    val input = s"\n\n\n\n\n\n\n\n$name"
    val is = new ByteArrayInputStream(input.getBytes)

    Console.withIn(is) {
      val result = SecretSantaImproved.getNames
      result should be (name)
    }
  }

  "randomSelect" should "return a single elem from a list" in {
    val input = List("a", "b", "c", "d", "e", "f")
    val result = SecretSantaImproved.randomSelect(input)

    result.isInstanceOf[String] should be (true)
    input.contains(result) should be (true)
  }

  it should "throw IllegalArgumentException for an empty list" in {
    an [IllegalArgumentException] should be thrownBy SecretSantaImproved.randomSelect(Nil)
    an [IllegalArgumentException] should be thrownBy SecretSantaImproved.randomSelect(List())
  }

  "pairNames" should "return Mapping of name pairs" in {
    val input = List("a", "b", "c", "d", "e", "f")
    val result = SecretSantaImproved.pairNames(input)

    result.isInstanceOf[Map[String, String]] should be (true)
    result.foreach { pair =>
      pair._1 != pair._2 should be (true)
    }
  }

  it should "return an empty Map for an empty input" in {
    SecretSantaImproved.pairNames(Nil) should be (Map())
    SecretSantaImproved.pairNames(List()) should be (Map())
  }
}
