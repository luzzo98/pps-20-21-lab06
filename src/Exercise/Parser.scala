package Exercise

abstract class Parser[T] {
  def parse(t: T): Boolean  // is the token accepted?
  def end(): Boolean        // is it ok to end here
  def parseAll(seq: Seq[T]): Boolean = (seq forall parse) & end() // note &, not &&
}

class BasicParser(chars: Set[Char]) extends Parser[Char] {
  override def parse(t: Char): Boolean = chars.contains(t)
  override def end(): Boolean = true
}

trait NonEmpty[T] extends Parser[T]{
  private[this] var empty = true
  abstract override def parse(t: T): Boolean = {empty = false; super.parse(t)} // who is super??
  abstract override def end(): Boolean = !empty && {empty = true; super.end()}
}

class NonEmptyParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

trait NotTwoConsecutive[T] extends Parser[T]{
  private[this] var consecutive = false
  private[this] var previous: Option[T] = Option.empty
  abstract override def parse(t: T): Boolean = consecutive match {
    case true => false
    case _ if previous.isDefined && t==previous.get => consecutive = true; false
    case _ => previous = Option(t); super.parse(t)
    }
}

class NotTwoConsecutiveParser(chars: Set[Char]) extends BasicParser(chars) with NotTwoConsecutive[Char]

object ParserFactory {
  implicit class ParserFromString(s: String) {
    def charParser(): Parser[Char] = new BasicParser(s.toSet)
  }
}

