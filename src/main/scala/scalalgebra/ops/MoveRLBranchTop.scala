package scalalgebra.ops

import scalalgebra.axioms.AlgebraAxioms.{Associative, Commutative}
import cats.Bifunctor
import scalalgebra.utils.DepFuncInv

object MoveRLBranchTopOps {

  /**
   * At type level, allows to "remove" an RL-branch and expose the rest XRest. The output
   * moves the RL-branch on the top of tree thus becoming the new main left-branch.
   *
   * You cannot remove the "tail". i.e. either a leaf, or a subtree. See MoveRSubtreeTop.
   * Requires Associative and Commutative.
   */

  // The concrete value to give is X, and you additionally need to give a type parameter RLB
  trait MoveRLBranchTop[F[_, _], X, RLB] extends DepFuncInv[X] {
    type XRest
    final type Out = F[RLB, XRest] // Out is fixed, you need to override XRest in the implicits.
  }

  object MoveRLBranchTop {
    type Aux[F[_, _], X, RLB, XRest0] = MoveRLBranchTop[F, X, RLB] {type XRest = XRest0}

    implicit def rec[F[_, _], L, R, RLB]
    (implicit rrlb: MoveRLBranchTop[F, R, RLB],
     ass: Associative[F],
     comm: Commutative[F],
     bif: Bifunctor[F],
    ): Aux[F, F[L, R], RLB, F[L, rrlb.XRest]] =
      new MoveRLBranchTop[F, F[L, R], RLB] {
        type XRest = F[L, rrlb.XRest]

        override def apply(in: F[L, R]): Out = {
          val x: F[L, F[RLB, rrlb.XRest]] = bif.rightFunctor.map(in)(rrlb.apply(_))
          val y: F[F[L, RLB], rrlb.XRest] = ass.shiftLeft(x)
          val z: F[F[RLB, L], rrlb.XRest] = bif.leftFunctor.map(y)(comm.swap)
          ass.shiftRight(z)
        }

        override def inverse(out: Out): F[L, R] = {
          val x: F[F[RLB, L], rrlb.XRest] = ass.shiftLeft(out)
          val y: F[F[L, RLB], rrlb.XRest] = bif.leftFunctor.map(x)(comm.swap)
          val z: F[L, F[RLB, rrlb.XRest]] = ass.shiftRight(y)
          bif.rightFunctor.map(z)(rrlb.inverse)
        }
      }
    
    implicit def base[F[_, _], RLB, R]: Aux[F, F[RLB, R], RLB, R] =
      new MoveRLBranchTop[F, F[RLB, R], RLB] {
        type XRest = R
        override def apply(in: F[RLB, R]): F[RLB, XRest] = in
        override def inverse(out: F[RLB, XRest]): F[RLB, R] = out
      }
  }

}
