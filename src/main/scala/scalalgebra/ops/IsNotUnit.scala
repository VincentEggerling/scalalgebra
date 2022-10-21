package scalalgebra.ops

import scalalgebra.axioms.AlgebraAxioms.Unitative

object IsNotUnitOps {

  /** Proves that A is not a unit for F */
  trait IsNotUnit[F[_, _], A]

  object IsNotUnit {
    type UnitativeAux[F[_, _], U] = Unitative[F] {type Unity = U}

    implicit def ambigu1[F[_, _], U](implicit un: UnitativeAux[F, U]): IsNotUnit[F, U] = new IsNotUnit[F, U] {}
    implicit def ambigu2[F[_, _], U](implicit un: UnitativeAux[F, U]): IsNotUnit[F, U] = new IsNotUnit[F, U] {}
    implicit def notUnit[F[_, _], A]: IsNotUnit[F, A] = new IsNotUnit[F, A] {}
  }

}
