import scala.annotation.tailrec

sealed trait NelN[T]
case class LastN[T](value: T) extends NelN[T]
case class ConstN[T](value: T, tail: NelN[T]) extends NelN[T]

object NelNSyntax {
  implicit class NelNOps[T](value: NelN[T]) {
    def reverse: NelN[T] = {
      @tailrec
      def loop(curr: NelN[T], acc: NelN[T]): NelN[T] = {
        curr match {
          case LastN(value) => ConstN(value, acc)
          case ConstN(value, tail) => loop(tail, ConstN(value, acc))
        }
      }
      value match {
        case last: LastN[T] => last
        case ConstN(value, tail) => loop(tail, LastN(value))
      }
    }

    def |+|(other: NelN[T])(implicit semigroupN: SemigroupN[NelN[T]]): NelN[T] = {
      semigroupN.combine(value, other)
    }
  }
}

object NelN {
  def apply[T](head: T, elements: T*): NelN[T] = {
    if (elements.nonEmpty) {
      val el: Seq[T] = elements.reverse
      ConstN(head, el.tail.foldLeft(LastN(el.head): NelN[T])((acc, e) => ConstN(e, acc)))
    } else {
      LastN(head)
    }
  }

}

object NelnInstances {
  import NelNSyntax._
  implicit def nelSemigroup[A] = new SemigroupN[NelN[A]] {
    override def combine(a: NelN[A], b: NelN[A]): NelN[A] = {
      @tailrec
      def loop(current: NelN[A], acc: NelN[A]): NelN[A] = {
        current match {
          case LastN(value) => ConstN(value, acc)
          case ConstN(value, tail) => loop(tail, ConstN(value, acc))
        }
      }
      loop(a.reverse, b)
    }
  }
}



object Main extends App {
  import NelNSyntax._
  import NelnInstances._

  val a1 = NelN(1,2,3,4,5)
  val a2 = NelN(6,7,8,9,10,11)
  val a3 = a1 |+| a2

  println(a1)
  println(a1.reverse)
  println(a3)
}

sealed trait TreeN[T]
case class LeaveN[T](value: T) extends TreeN[T]
case class NodeN[T](value: T, left: TreeN[T], right: TreeN[T]) extends TreeN[T]
