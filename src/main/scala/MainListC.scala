import scala.annotation.tailrec

sealed trait ListC[+A]
case class ConstC[A](head: A, tail: ListC[A]) extends ListC[A]
case object NilC extends ListC[Nothing]

object ListC {
  def apply[T](values: T*): ListC[T] = {
    if (values.isEmpty)
      NilC
    else
//      Cons(as.head, apply(as.tail: _*))
      values.foldRight(NilC: ListC[T])((e, acc) => ConstC(e, acc) )
  }

  def reverse[T](value: ListC[T]): ListC[T] = {
    @tailrec
    def loop(curr: ListC[T], acc: ListC[T]): ListC[T] = {
      curr match {
        case NilC => acc
        case ConstC(head, tail) => loop(tail, ConstC(head, acc))
      }
    }
    value match {
      case NilC => NilC
      case ConstC(head, tail) => loop(tail, ConstC(head, NilC))
    }
  }
}

object ListCInstances {
  implicit def semigroupN[T] = new SemigroupN[ListC[T]] {
    override def combine(x: ListC[T], y: ListC[T]): ListC[T] = {
      @tailrec
      def loop(curr: ListC[T], acc: ListC[T]): ListC[T] = {
        curr match {
          case ConstC(head, tail) => loop(tail, ConstC(head, acc))
          case NilC => acc
        }
      }
      loop(ListC.reverse(x), y)
    }
  }

  implicit def monadN: MonadN[ListC] = new MonadN[ListC] {
    override def flatMap[A, B](value: ListC[A])(f: A => ListC[B]): ListC[B] =
      value match {
        case ConstC(head, tail) =>
          f(head) match {
            case ConstC(head, _) => ConstC(head, flatMap(tail)(f))
            case NilC => flatMap(tail)(f)
          }
        case NilC => NilC
      }

    override def pure[A](a: A): ListC[A] = ConstC(a, NilC)

    override def map[A, B](fa: ListC[A])(f: A => B): ListC[B] =
    //      fa match {
    //        case ConstC(head, tail) => ConstC(f(head), map(tail)(f))
    //        case NilC => NilC
    //      }
      flatMap(fa)(a => pure(f(a)))

    override def ap[A, B](ff: ListC[A => B])(fa: ListC[A]): ListC[B] = ???

  }
}

object ListCSyntax {
  implicit class ListCOps[T](value: ListC[T]) {

    def |+|(other: ListC[T])(implicit semigroupN: SemigroupN[ListC[T]]): ListC[T] = {
      semigroupN.combine(value, other)
    }

    def map[B](f: T => B)(implicit monadN: MonadN[ListC]) = {
      monadN.map(value)(f)
    }

    def flatMap[B](f: T => ListC[B])(implicit monadN: MonadN[ListC]) = {
      monadN.flatMap(value)(f)
    }
  }
}

object MainListC extends App {
  import ListCInstances._
  import ListCSyntax._
  val a1 = ListC(1,2,3)
  val a2 = ListC(6,7,8,9)

  val a3 = a1 |+| a2
  println(a3)

  val a4 = a1.map(_ * 3)
  println(a4)
}
