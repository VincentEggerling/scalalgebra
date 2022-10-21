package scalalgebra.ops

import scalalgebra.axioms.AlgebraAxioms.Commutative
import cats.Bifunctor
import scalalgebra.utils.Iso

object CommuteTreeOps {

  /**
   * Proof that X is equivalent to Y if you allow commutativity only.
   */
  trait CommuteTree[X, Y] extends Iso[X, Y]
  trait LowPrio {
    implicit def commuteTree[F[_, _], XL, XR, Y](implicit ct: CommuteTree_[F, XL, XR, Y])
    = new CommuteTree[F[XL, XR], Y] {
      override def to(in: F[XL, XR]): Y = ct.to(in)
      override def from(out: Y): F[XL, XR] = ct.from(out)
    }
  }
  
  object CommuteTree extends LowPrio {
    implicit def trivial[A] = new CommuteTree[A, A] {
      override def to(in: A): A = in
      override def from(out: A): A = out
    }
  }


  protected sealed trait CommuteTree_[F[_, _], XL, XR, Y] extends Iso[F[XL, XR], Y]
  trait Prio0 {
    implicit def baseNoSwap[F[_, _], A, B] = new CommuteTree_[F, A, B, F[A, B]] {
      override def to(in: F[A, B]): F[A, B] = in
      override def from(out: F[A, B]): F[A, B] = out
    }
    implicit def baseSwap[F[_, _], A, B](implicit comm: Commutative[F]) = new CommuteTree_[F, A, B, F[B, A]] {
      override def to(in: F[A, B]): F[B, A] = comm.swap(in)
      override def from(out: F[B, A]): F[A, B] = comm.swap(out)
    }
  }

  trait Prio1 extends Prio0 {
    implicit def recLNoSwap[F[_, _], R, LL, LR, LY]
    (implicit e: CommuteTree_[F, LL, LR, LY], bif: Bifunctor[F])
    = new CommuteTree_[F, F[LL, LR], R, F[LY, R]] {
      override def to(in: F[F[LL, LR], R]): F[LY, R] = bif.leftFunctor.map(in)(e.to)
      override def from(out: F[LY, R]): F[F[LL, LR], R] = bif.leftFunctor.map(out)(e.from)
    }

    implicit def recLSwap[F[_, _], R, LL, LR, LY]
    (implicit e: CommuteTree_[F, LL, LR, LY], comm: Commutative[F], bif: Bifunctor[F])
    = new CommuteTree_[F, F[LL, LR], R, F[R, LY]] {
      override def to(in: F[F[LL, LR], R]): F[R, LY] = comm.swap(bif.leftFunctor.map(in)(e.to))
      override def from(out: F[R, LY]): F[F[LL, LR], R] = bif.leftFunctor.map(comm.swap(out))(e.from)
    }
  }

  trait Prio2 extends Prio1 {
    implicit def recRNoSwap[F[_, _], L, RL, RR, RY]
    (implicit e: CommuteTree_[F, RL, RR, RY], bif: Bifunctor[F])
    = new CommuteTree_[F, L, F[RL, RR], F[L, RY]] {
      override def to(in: F[L, F[RL, RR]]): F[L, RY] = bif.rightFunctor.map(in)(e.to)
      override def from(out: F[L, RY]): F[L, F[RL, RR]] = bif.rightFunctor.map(out)(e.from)
    }

    implicit def recRSwap[F[_, _], L, RL, RR, RY]
    (implicit e: CommuteTree_[F, RL, RR, RY], comm: Commutative[F], bif: Bifunctor[F])
    = new CommuteTree_[F, L, F[RL, RR], F[RY, L]] {
      override def to(in: F[L, F[RL, RR]]): F[RY, L] = comm.swap(bif.rightFunctor.map(in)(e.to))
      override def from(out: F[RY, L]): F[L, F[RL, RR]] = bif.rightFunctor.map(comm.swap(out))(e.from)
    }
  }

  object CommuteTree_ extends Prio2 {
    implicit def recLRNoSwap[F[_, _], LL, LR, RL, RR, LY, RY]
    (implicit cl: CommuteTree_[F, LL, LR, LY], cr: CommuteTree_[F, RL, RR, RY], bif: Bifunctor[F])
    = new CommuteTree_[F, F[LL, LR], F[RL, RR], F[LY, RY]] {
      override def to(in: F[F[LL, LR], F[RL, RR]]): F[LY, RY] = bif.bimap(in)(cl.to, cr.to)
      override def from(out: F[LY, RY]): F[F[LL, LR], F[RL, RR]] = bif.bimap(out)(cl.from, cr.from)
    }

    implicit def recLRSwap[F[_, _], LL, LR, RL, RR, LY, RY]
    (implicit cl: CommuteTree_[F, LL, LR, LY], cr: CommuteTree_[F, RL, RR, RY], comm: Commutative[F], bif: Bifunctor[F])
    = new CommuteTree_[F, F[LL, LR], F[RL, RR], F[RY, LY]] {
      override def to(in: F[F[LL, LR], F[RL, RR]]): F[RY, LY] = comm.swap(bif.bimap(in)(cl.to, cr.to))
      override def from(out: F[RY, LY]): F[F[LL, LR], F[RL, RR]] = bif.bimap(comm.swap(out))(cl.from, cr.from)
    }
  }
}
