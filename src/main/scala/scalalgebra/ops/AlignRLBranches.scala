package scalalgebra.ops

import scalalgebra.ops.MoveRLBranchTopOps.MoveRLBranchTop
import scalalgebra.ops.MoveRSubtreeTopOps.MoveRSubtreeTop
import cats.Bifunctor
import scalalgebra.utils.Iso


object AlignRLBranchesOps {

  /**
   * Proof that when you swap the RL branches on X you can get the tree Y.
   * An RL branch is defined as a left branch starting from the main right branch from the root.
   *  - The top level left branch is also on RL branch
   *  - Any subtree that start on the main right branch is also considered as an RL branch. Sounds weird
   *    but make sense for the operation.
   */
  trait AlignRLBranches[X, Y] extends Iso[X, Y]

  trait Prio0 {
    implicit def recRLBranch[F[_, _], RLB, X, XRest, Y]
    (implicit moveRLbranch: MoveRLBranchTop.Aux[F, X, RLB, XRest],
     align: AlignRLBranches[XRest, Y],
     bif: Bifunctor[F])
    = new AlignRLBranches[X, F[RLB, Y]] {
      override def to(in: X): F[RLB, Y] = bif.rightFunctor.map(moveRLbranch.apply(in))(align.to)
      override def from(out: F[RLB, Y]): X = moveRLbranch.inverse(bif.rightFunctor.map(out)(align.from))
    }

    implicit def recSubtree[F[_, _], Subtree, X, XRest, Y]
    (implicit moveSubtree: MoveRSubtreeTop.Aux[F, X, Subtree, XRest],
     align: AlignRLBranches[XRest, Y],
     bif: Bifunctor[F])
    = new AlignRLBranches[X, F[Subtree, Y]] {
      override def to(in: X): F[Subtree, Y] = bif.rightFunctor.map(moveSubtree.apply(in))(align.to)
      override def from(out: F[Subtree, Y]): X = moveSubtree.inverse(bif.rightFunctor.map(out)(align.from))
    }

  }
  object AlignRLBranches extends Prio0 {
    implicit def trivial[A] = new AlignRLBranches[A, A] {
      override def to(in: A): A = in
      override def from(out: A): A = out
    }
  }
}
