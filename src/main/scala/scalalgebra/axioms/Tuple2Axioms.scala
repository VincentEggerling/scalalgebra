package scalalgebra.axioms

import cats.Bifunctor
import scalalgebra.axioms.AlgebraStructureAxioms.CommutativeMonoid
import scalalgebra.utils.Iso

object Tuple2Axioms {
  implicit val tuple2Axioms = new CommutativeMonoid[Tuple2] with Bifunctor[Tuple2] {
    override def shiftRight[A, B, C](fabc: ((A, B), C)): (A, (B, C)) = (fabc._1._1, (fabc._1._2, fabc._2))

    override def shiftLeft[A, B, C](fabc: (A, (B, C))): ((A, B), C) = ((fabc._1, fabc._2._1), fabc._2._2)

    override def swap[A, B](fab: (A, B)): (B, A) = fab.swap


    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) = (f(fab._1), g(fab._2))

    override type Unity = Unit

    override def leftUnitIso[A]: Iso[(A, Unity), A] = new Iso[(A, Unity), A] {
      override def to(in: (A, Unity)): A = in._1

      override def from(out: A): (A, Unity) = (out, ())
    }

    override def rightUnitIso[A]: Iso[(Unity, A), A] = new Iso[(Unity, A), A] {
      override def to(in: (Unity, A)): A = in._2

      override def from(out: A): (Unity, A) = ((), out)
    }
  }

}
