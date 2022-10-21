package scalalgebra.axioms

import scalalgebra.axioms.AlgebraAxioms.{Associative, Commutative, Unitative}
import scalalgebra.utils.Iso

object AlgebraAxioms {
  trait Associative[F[_, _]] {
    def shiftRight[A, B, C](fabc: F[F[A, B], C]): F[A, F[B, C]]
    def shiftLeft[A, B, C](fabc: F[A, F[B, C]]): F[F[A, B], C]
  }

  trait Commutative[F[_, _]] {
    def swap[A, B](fab: F[A, B]): F[B, A]
  }

  trait Unitative[F[_, _]] {
    type Unity
    def leftUnitIso[A]: Iso[F[A, Unity], A]
    def rightUnitIso[A]: Iso[F[Unity, A], A]
  }
}

object AlgebraStructureAxioms {

  /* TODO Maybe create implicits to generate the trait based on the Basic axioms.
     Say a function requires Monoid and you have Associative and Unitative in scope, it wouldn't work.
     Though no function currently requires the structure trait as implicit parameters.
   */

  trait Magma[F[_, _]]

  trait UnitativeMagma[F[_, _]] extends Magma[F] with Unitative[F]
  trait Semigroup[F[_, _]] extends Magma[F] with Associative[F]
  trait CommutativeMagma[F[_, _]] extends Magma[F] with Commutative[F]

  trait Monoid[F[_, _]] extends Semigroup[F] with UnitativeMagma[F]
  trait CommutativeUnitativeMagma[F[_, _]] extends UnitativeMagma[F] with CommutativeMagma[F]
  trait CommutativeSemigroup[F[_, _]] extends Semigroup[F] with CommutativeMagma[F]

  trait CommutativeMonoid[F[_, _]] extends Monoid[F] with CommutativeUnitativeMagma[F] with CommutativeSemigroup[F]
}
