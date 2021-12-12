import scala.annotation.tailrec

sealed trait Nel[T]
case class LastN[T](value: T) extends Nel[T]
case class ConstN[T](head: T, tail: Nel[T]) extends Nel[T]


object Nel {
  def apply[T](head: T, elements: T*): Nel[T] = {
    if (elements.nonEmpty) {
      val el: Seq[T] = elements.reverse
      ConstN(head, el.tail.foldLeft(LastN(el.head): Nel[T])((acc, e) => ConstN(e, acc)))
    } else {
      LastN(head)
    }
  }

  // TODO find a better place
  def reverse[T](value: Nel[T]): Nel[T] = {
    @tailrec
    def loop(curr: Nel[T], acc: Nel[T]): Nel[T] = {
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

}


object NelInstances {
  implicit def nelSemigroup[A] = new SemigroupN[Nel[A]] {
    override def combine(a: Nel[A], b: Nel[A]): Nel[A] = {
      @tailrec
      def loop(current: Nel[A], acc: Nel[A]): Nel[A] = {
        current match {
          case LastN(value) => ConstN(value, acc)
          case ConstN(value, tail) => loop(tail, ConstN(value, acc))
        }
      }
      loop(Nel.reverse(a), b)
    }
  }

  implicit def nelMonad : MonadN[Nel] = new MonadN[Nel] {
    override def flatMap[A, B](value: Nel[A])(f: A => Nel[B]): Nel[B] =
      value match {
        case ConstN(head, tail) =>
          f(head) match {
            case ConstN(head, _) => ConstN(head, flatMap(tail)(f))
            case LastN(last) => ConstN(last, flatMap(tail)(f))
          }
        case LastN(value) => f(value)
    }

    override def pure[A](a: A): Nel[A] = LastN(a)

    override def ap[A, B](ff: Nel[A => B])(fa: Nel[A]): Nel[B] = ???

    override def map[A, B](fa: Nel[A])(f: A => B): Nel[B] =
      flatMap(fa)(a => pure(f(a)))
//      fa match {
//      case LastN(value) => LastN(f(value))
//      case ConstN(head, tail) => ConstN(f(head), map(tail)(f))
//    }
  }
}

object NelSyntax {
  implicit class NelNOps[T](value: Nel[T]) {

    def |+|(other: Nel[T])(implicit semigroupN: SemigroupN[Nel[T]]): Nel[T] = {
      semigroupN.combine(value, other)
    }

    def map[B](f: T => B)(implicit monadN: MonadN[Nel]) = {
      monadN.map(value)(f)
    }

    def flatMap[B](f: T => Nel[B])(implicit monadN: MonadN[Nel]) = {
      monadN.flatMap(value)(f)
    }
  }
}

object MainNel extends App {
  import NelSyntax._
  import NelInstances._

  val a1 = Nel(1,2,3)
  val a2 = Nel(6,7,8,9)

  val a3 = a1 |+| a2
  println(a3)

  val a4 = a1.map(_ * 3)
  println(a4)
}

