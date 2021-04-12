package Exercise

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = {
    var res = 0.0
    a.foreach(res+=_)
    res
  }

  override def concat(a: Seq[String]): String = {
    var res = ""
    a.foreach(s => res=res.concat(s))
    res
  }

  override def max(a: List[Int]): Int = {
    var res = Int.MinValue
    a.foreach(v => if (v>res) res=v)
    res
  }

  def combine[A](c: Seq[A])(implicit combiner: Combiner[A]): A = {
    var res = combiner.unit
    c.foreach(v => res=combiner.combine(res,v))
    res
  }
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object CombinerObjects {
  implicit case object CombinerSum extends Combiner[Double] {
    override def unit: Double = 0.0
    override def combine(a: Double, b: Double): Double = a+b
  }

  implicit case object CombinerConcat extends Combiner[String] {
    override def unit: String = ""
    override def combine(a: String, b: String): String = a.concat(b)
  }

  implicit case object CombinerMax extends Combiner[Int] {
    override def unit: Int = Int.MinValue
    override def combine(a: Int, b: Int): Int = if (a>b) a else b
  }
}
