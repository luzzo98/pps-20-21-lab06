package TestExercise

import Exercise._
import Exercise.CombinerObjects._ // note this import
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions._

class TestCombiner {

  @Test
  def testFunctions() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001)
    assertEquals(0.0, f.sum(List()))
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.max(List()))
  }

  @Test
  def testCombiner(): Unit = {
    val f = FunctionsImpl
    assertEquals(60.1, f.combine(List(10.0, 20.0, 30.1))(CombinerSum), 0.001)
    assertEquals(0.0, f.combine(List())(CombinerSum))
    assertEquals("abc", f.combine(Seq("a", "b", "c"))(CombinerConcat))
    assertEquals("", f.combine(Seq())(CombinerConcat))
    assertEquals(3, f.combine(List(-10, 3, -5, 0))(CombinerMax))
    assertEquals(Integer.MIN_VALUE, f.combine(List())(CombinerMax))
  }

  @Test
  def testImplicitCombiner(): Unit = {
    val f = FunctionsImpl
    assertEquals(60.1, f.combine(List(10.0, 20.0, 30.1)), 0.001)
    assertEquals(0.0, f.combine(List[Double]())) // if the list is empty it must be of Double to check the type inference
    assertEquals("abc", f.combine(Seq("a", "b", "c")))
    assertEquals("", f.combine[String](Seq())) // you can also cast the method
    assertEquals(3, f.combine(List(-10, 3, -5, 0)))
    assertEquals(Integer.MIN_VALUE, f.combine(List[Int]()))
  }
}