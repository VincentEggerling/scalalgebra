package scalalgebra.ops

import scalalgebra.ops.MoveRBranchToLRLeafOps.MoveRBranchToLRLeaf
import cats.Bifunctor
import scalalgebra.utils.DepFuncInv
import scalalgebra.utils.KindsPredicatesOps.IsK0


object FlattenRightOps {

  /**
   * Compute the right-associated form of the expression.
   */
  trait FlattenRight[X] extends DepFuncInv[X]

  object FlattenRight {
    type Aux[X, Out0] = FlattenRight[X] {type Out = Out0}

    implicit def flattenRight[F[_, _], XL, XR](implicit e: FlattenRight_[F, XL, XR]): Aux[F[XL, XR], e.Out]
    = new FlattenRight[F[XL, XR]] {
      type Out = e.Out
      override def apply(in: F[XL, XR]): Out = e.apply(in)
      override def inverse(out: Out): F[XL, XR] = e.inverse(out)
    }

    implicit def trivial[A](implicit k0: IsK0[A]): Aux[A, A] = new FlattenRight[A] {
      type Out = A
      override def apply(in: A): Out = in
      override def inverse(out: Out): A = out
    }
  }

  protected sealed trait FlattenRight_[F[_, _], XL, XR] extends DepFuncInv[F[XL, XR]]
  trait Prio0 {
    type Aux[F[_, _], XL, XR, Out0] = FlattenRight_[F, XL, XR] {type Out = Out0}

    implicit def base[F[_, _], L, R](implicit k0: IsK0[L], k0_2: IsK0[R]): Aux[F, L, R, F[L, R]] =
      new FlattenRight_[F, L, R] {
        type Out = F[L, R]
        override def apply(in: F[L, R]): Out = in
        override def inverse(out: Out): F[L, R] = out
      }

    // When the tree has one leaf L on the left, and some tree R on the right. Assuming the tree R is already flattened to Y,
    // then the whole tree is flattened.
    implicit def recR[F[_, _], L, RL, RR, RY]
    (implicit fr: Aux[F, RL, RR, RY], bif: Bifunctor[F]): Aux[F, L, F[RL, RR], F[L, RY]] =
      new FlattenRight_[F, L, F[RL, RR]] {
        type Out = F[L, RY]
        override def apply(in: F[L, F[RL, RR]]): Out = bif.rightFunctor.map(in)(fr.apply)
        override def inverse(out: Out): F[L, F[RL, RR]] = bif.rightFunctor.map(out)(fr.inverse)
      }

    // When the tree has one leaf R on the right, and some tree L on the left. Assuming the tree L is already flattened to Y,
    // We need to append R at the far right of Y
    implicit def recL[F[_, _], LL, LR, R, LY]
    (implicit fl: Aux[F, LL, LR, LY], concat: MoveRBranchToLRLeaf[F[LY, R]], bif: Bifunctor[F]): Aux[F, F[LL, LR], R, concat.Out] =
      new FlattenRight_[F, F[LL, LR], R] {
        type Out = concat.Out
        override def apply(in: F[F[LL, LR], R]): Out = concat.apply(bif.leftFunctor.map(in)(fl.apply))
        override def inverse(out: Out): F[F[LL, LR], R] = bif.leftFunctor.map(concat.inverse(out))(fl.inverse)
      }
  }

  object FlattenRight_ extends Prio0 {
    // When the tree has two subtree L, R, and we assume that both are flattened to YL, YR Then we can combine them by appending
    // YR to the right most leaves of YL.
    implicit def recLR[F[_, _], LL, LR, RL, RR, YL, YR]
    (implicit fl: Aux[F, LL, LR, YL], fr: Aux[F, RL, RR, YR], concat: MoveRBranchToLRLeaf[F[YL, YR]], bif: Bifunctor[F]): Aux[F, F[LL, LR], F[RL, RR], concat.Out] =
      new FlattenRight_[F, F[LL, LR], F[RL, RR]] {
        type Out = concat.Out
        override def apply(in: F[F[LL, LR], F[RL, RR]]): Out = concat.apply(bif.bimap(in)(fl.apply, fr.apply))
        override def inverse(out: Out): F[F[LL, LR], F[RL, RR]] = bif.bimap(concat.inverse(out))(fl.inverse, fr.inverse)
      }
  }
}
