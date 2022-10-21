package scalalgebra.ops

import scalalgebra.axioms.AlgebraAxioms.{Associative, Commutative}
import cats.Bifunctor
import scalalgebra.utils.{DepFuncInv}


object MoveRSubtreeTopOps {

  /** Proves that there is a Subtree starting at some node in the main right branch of X
   * and RestX is all what's above.
   * The DepFunc brings it to the top left.
   * Note that you cannot remove the whole tree, i.e RemoveRSubtree[F, X, X] is not defined.
   */

  trait MoveRSubtreeTop[F[_, _], X, SubT] extends DepFuncInv[X] {
    type RestX
    final type Out = F[SubT, RestX]
  }

  object MoveRSubtreeTop {
    type Aux[F[_, _], X, SubT, RestX0] = MoveRSubtreeTop[F, X, SubT] {type RestX = RestX0}

    implicit def rec[F[_, _], XL, XR, Sub]
    (implicit rst: MoveRSubtreeTop[F, XR, Sub],
     ass: Associative[F],
     comm: Commutative[F],
     bif: Bifunctor[F]
    ): Aux[F, F[XL, XR], Sub, F[XL, rst.RestX]] =
      new MoveRSubtreeTop[F, F[XL, XR], Sub] {
        type RestX = F[XL, rst.RestX]

        override def apply(in: F[XL, XR]): Out = {
          val x: F[XL, F[Sub, rst.RestX]] = bif.rightFunctor.map(in)(rst.apply(_))
          val y: F[F[XL, Sub], rst.RestX] = ass.shiftLeft(x)
          val z: F[F[Sub, XL], rst.RestX] = bif.leftFunctor.map(y)(comm.swap)
          ass.shiftRight(z)
        }
        override def inverse(out: Out): F[XL, XR] = {
          val x: F[F[Sub, XL], rst.RestX] = ass.shiftLeft(out)
          val y: F[F[XL, Sub], rst.RestX] = bif.leftFunctor.map(x)(comm.swap)
          val z: F[XL, F[Sub, rst.RestX]] = ass.shiftRight(y)
          bif.rightFunctor.map(z)(rst.inverse)
        }
      }

    implicit def base[F[_, _], X, Sub]
    (implicit comm: Commutative[F]): Aux[F, F[X, Sub], Sub, X] =
      new MoveRSubtreeTop[F, F[X, Sub], Sub] {
        type RestX = X
        override def apply(in: F[X, Sub]): Out = comm.swap(in)
        override def inverse(out: Out): F[X, Sub] = comm.swap(out)
      }
  }
}
