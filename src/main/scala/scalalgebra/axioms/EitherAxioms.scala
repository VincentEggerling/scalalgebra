package scalalgebra.axioms

import cats.Bifunctor
import scalalgebra.axioms.AlgebraStructureAxioms.CommutativeMonoid
import scalalgebra.utils.Bottom.Bottom
import scalalgebra.utils.Iso

object EitherAxioms {
  implicit val eitherAxioms = new CommutativeMonoid[Either] with Bifunctor[Either] {

    override def shiftRight[A, B, C](fabc: Either[Either[A, B], C]): Either[A, Either[B, C]] = fabc match {
      case Left(eitherAB) => eitherAB match {
        case Left(a) => Left(a)
        case Right(b) => Right(Left(b))
      }
      case Right(c) => Right(Right(c))
    }

    override def shiftLeft[A, B, C](fabc: Either[A, Either[B, C]]): Either[Either[A, B], C] = fabc match {
      case Left(a) => Left(Left(a))
      case Right(eitherBC) => eitherBC match {
        case Left(b) => Left(Right(b))
        case Right(c) => Right(c)
      }
    }

    override type Unity = Bottom

    override def leftUnitIso[A]: Iso[Either[A, Unity], A] = new Iso[Either[A, Unity], A] {
      override def to(in: Either[A, Unity]): A = in match {
        case Left(a) => a
        case Right(_) => throw new Exception("Either unit doesn't exist")
      }

      override def from(out: A): Either[A, Unity] = Left(out)
    }

    override def rightUnitIso[A]: Iso[Either[Unity, A], A] = new Iso[Either[Unity, A], A] {
      override def to(in: Either[Unity, A]): A = in match {
        case Left(_) => throw new Exception("Either unit doesn't exist")
        case Right(a) => a
      }

      override def from(out: A): Either[Unity, A] = Right(out)
    }

    override def swap[A, B](fab: Either[A, B]): Either[B, A] = fab match {
      case Left(a) => Right(a)
      case Right(b) => Left(b)
    }

    override def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] = implicitly[Bifunctor[Either]].bimap(fab)(f, g)
  }

}
