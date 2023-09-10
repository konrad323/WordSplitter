package plugin

import exception.{EmptyTextException, InvalidCharactersLimit, WordLengthGreaterThanLimit}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SplitterTest extends AnyFunSuite with Matchers {

  test("Empty text exception should be returned when passed as argument") {
    val result = Splitter.split("", 5)
    result shouldBe Left(EmptyTextException())
  }

  test("Invalid characters limit should be returned when zero passed as argument") {
    val result = Splitter.split("text", 0)
    result shouldBe Left(InvalidCharactersLimit())
  }

  test("Invalid characters limit should be returned when negative number passed as argument") {
    val result = Splitter.split("text", -10)
    result shouldBe Left(InvalidCharactersLimit())
  }

  test("Word length greater than limit should be returned when invalid arguments passed") {
    val text = "text"
    val result = Splitter.split(text, 2)
    result shouldBe Left(WordLengthGreaterThanLimit())
  }

  test("Text should be split correctly when its size == perLine limit") {
    val text = "Text"
    val expectedResult = "Text"
    val result = Splitter.split(text, 4)
    result shouldBe Right(expectedResult)
  }

  test("Text should be split correctly when one word size == perLine") {
    val text = "Text me"
    val expectedResult = "Text\nme"
    val result = Splitter.split(text, 4)
    result shouldBe Right(expectedResult)
  }

  test("Text should be split correctly when all words size != perLine") {
    val text = "Text me tomorrow"
    val expectedResult = "Text me\ntomorrow"
    val result = Splitter.split(text, 10)
    result shouldBe Right(expectedResult)
  }

  test("Text should be split correctly when all words size == perLine") {
    val text = "Text text text"
    val expectedResult = "Text\ntext\ntext"
    val result = Splitter.split(text, 4)
    result shouldBe Right(expectedResult)
  }

  test("Text should not be split when its total size smaller than perLine limit") {
    val text = "Text text"
    val expectedResult = "Text text"
    val result = Splitter.split(text, 10)
    result shouldBe Right(expectedResult)
  }

  test("Text with additional spaces should be split correctly") {
    val text = "Text    me"
    val expectedResult = "Text\nme"
    val result = Splitter.split(text, 4)
    result shouldBe Right(expectedResult)
  }

  test("Long size text should be split correctly") {
    val text = "In 1991, while studying computer science at University of Helsinki, Linus Torvalds began a project that later became the Linux kernel. He wrote the program specifically for the hardware he was using and independent of an operating system because he wanted to use the functions of his new PC with an 80386 processor. Development was done on MINIX using the GNU C Compiler."
    val expectedResult = "In 1991, while studying computer science\nat University of Helsinki, Linus\nTorvalds began a project that later\nbecame the Linux kernel. He wrote the\nprogram specifically for the hardware he\nwas using and independent of an\noperating system because he wanted to\nuse the functions of his new PC with an\n80386 processor. Development was done on\nMINIX using the GNU C Compiler."
    val result = Splitter.split(text, 40)
    result shouldBe Right(expectedResult)
  }
}
