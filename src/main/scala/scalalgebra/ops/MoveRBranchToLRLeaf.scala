package scalalgebra.ops


import scalalgebra.axioms.AlgebraAxioms.Associative
import cats.Bifunctor
import scalalgebra.utils.DepFuncInv
import scalalgebra.utils.KindsPredicatesOps.IsK0

object MoveRBranchToLRLeafOps {

  /**
   * Take the R branch and moves it at the end of the first LR branch. Putting the right leaf
   * of the LR branch on the left and appending the R branch on the right.
   */
  trait MoveRBranchToLRLeaf[X] extends DepFuncInv[X]

  object MoveRBranchToLRLeaf {
    type Aux[X, Out0] = MoveRBranchToLRLeaf[X] {type Out = Out0}

    implicit def trivial[X](implicit neq1: IsK0[X]): Aux[X, X] =
      new MoveRBranchToLRLeaf[X] {
        type Out = X
        override def apply(in: X): Out = in
        override def inverse(out: Out): X = out
      }

    implicit def moveRBranchToLRLeaf[F[_, _], XL, XR](implicit alr: MoveRBranchToLRLeaf_[F, XL, XR]): Aux[F[XL, XR], alr.Out]
    = new MoveRBranchToLRLeaf[F[XL, XR]] {
      type Out = alr.Out
      override def apply(in: F[XL, XR]): Out = alr.apply(in)
      override def inverse(out: Out): F[XL, XR] = alr.inverse(out)
    }
    
  }

  // Might seem over complicated to do that with two proof trait. But it actually helps
  // the implicit class AlgebraOps for the inverse() method.
  protected sealed trait MoveRBranchToLRLeaf_[F[_, _], XL, XR] extends DepFuncInv[F[XL, XR]]
  trait Prio0 {
    type Aux[F[_, _], XL, XR, Out0] = MoveRBranchToLRLeaf_[F, XL, XR] {type Out = Out0}

    implicit def base[F[_, _], XL, XR](implicit k0: IsK0[XL]): Aux[F, XL, XR, F[XL, XR]] =
      new MoveRBranchToLRLeaf_[F, XL, XR] {
        type Out = F[XL, XR]
        override def apply(in: F[XL, XR]): Out = in
        override def inverse(out: Out): F[XL, XR] = out
      }
  }
  object MoveRBranchToLRLeaf_ extends Prio0 {

    implicit def rec[F[_, _], LL, LR, R]
    (implicit c: MoveRBranchToLRLeaf_[F, LR, R], bif: Bifunctor[F], ass: Associative[F]): Aux[F, F[LL, LR], R, F[LL, c.Out]] =
      new MoveRBranchToLRLeaf_[F, F[LL, LR], R] {
        type Out = F[LL, c.Out]
        override def apply(in: F[F[LL, LR], R]): Out = bif.rightFunctor.map(ass.shiftRight(in))(c.apply(_))
        override def inverse(out: Out): F[F[LL, LR], R] = ass.shiftLeft(bif.rightFunctor.map(out)(c.inverse))
      }
  }
}
