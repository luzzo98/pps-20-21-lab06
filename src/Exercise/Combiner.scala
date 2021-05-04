package Exercise

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = {
    a.foldRight(0.0)(_+_)

    // first implementation
//    var res = 0.0
//    a.foreach(res+=_)
//    res
  }

  override def concat(a: Seq[String]): String = {
    a.foldRight("")(_.concat(_))

    // first implementation
//    var res = ""
//    a.foreach(s => res=res.concat(s))
//    res
  }

  override def max(a: List[Int]): Int = {
    a.foldRight(Int.MinValue)((a,b) => if (a>b) a else b)

    // first implementation
//    var res = Int.MinValue
//    a.foreach(v => if (v>res) res=v)
//    res
  }

  // method added in the second point of step 2
  def combine[T](a: Seq[T])(implicit combiner: Combiner[T]): T = {
    a.foldRight(combiner.unit)((a,b) => combiner.combine(a,b))

    // first implementation
//    var res = combiner.unit
//    a.foreach(v => res=combiner.combine(res,v))
//    res
  }
}

trait Combiner[T] {
  def unit: T
  def combine(a: T, b: T): T
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
