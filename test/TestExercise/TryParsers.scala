package TestExercise

import Exercise._
import Exercise.ParserFactory.ParserFromString
import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class TryParsers {

  @Test
  def testBasicParser(): Unit = {
    def parser = new BasicParser(Set('a', 'b', 'c'))

    assertTrue(parser.parseAll("aabc".toList))
    assertFalse(parser.parseAll("aabcdc".toList))
    assertTrue(parser.parseAll("".toList))
  }

  @Test
  def testNotEmptyParser(): Unit = {
    def parserNE = new NonEmptyParser(Set('0', '1'))

    assertTrue(parserNE.parseAll("0101".toList))
    assertFalse(parserNE.parseAll("0123".toList))
    assertFalse(parserNE.parseAll(List()))
  }

  @Test
  def testNotTwoConsecutiveParser(): Unit = {
    def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))

    assertTrue(parserNTC.parseAll("XYZ".toList))
    assertFalse(parserNTC.parseAll("XYYZ".toList))
    assertTrue(parserNTC.parseAll("".toList))
  }

  @Test
  def testNotEmptyAndNotTwoConsecutiveParser(): Unit = {
    def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]

    assertTrue(parserNTCNE.parseAll("XYZ".toList))
    assertFalse(parserNTCNE.parseAll("XYYZ".toList))
    assertFalse(parserNTCNE.parseAll("".toList))
  }

  @Test
  def testStringParser(): Unit = {
    def sparser: Parser[Char] = "abc".charParser()

    assertTrue(sparser.parseAll("aabc".toList))
    assertFalse(sparser.parseAll("aabcdc".toList))
    assertTrue(sparser.parseAll("".toList))
  }
}


