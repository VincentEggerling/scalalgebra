package scalalgebra.ops

import cats.Bifunctor
import scalalgebra.utils.{Iso, IsoK2}

object MapTreeK2Ops {

  /**
   * Given an implicit IsoK2 between two bifunctors, allows to convert the tree to the other functor.
   * TODO: We have a compilation performance issue with this implicit pattern with type bound <: F[_,_]
   */
  trait MapTreeK2[FX, GY] extends Iso[FX, GY]

  trait Prio0 {
    implicit def base[F[_, _], G[_, _], L, R](implicit baseIso: IsoK2[F, G]) = new MapTreeK2[F[L, R], G[L, R]] {
      override def to(in: F[L, R]): G[L, R] = baseIso.to(in)
      override def from(out: G[L, R]): F[L, R] = baseIso.from(out)
    }
  }
  trait Prio1 extends Prio0 {
    implicit def recR[F[_, _], G[_, _], L, FR <: F[_, _], GR <: G[_, _]]
    (implicit baseIso: IsoK2[F, G], bif: Bifunctor[F], big: Bifunctor[G], tt: MapTreeK2[FR, GR]) =
      new MapTreeK2[F[L, FR], G[L, GR]] {
        override def to(in: F[L, FR]): G[L, GR] = baseIso.to(bif.rightFunctor.map(in)(tt.to))
        override def from(out: G[L, GR]): F[L, FR] = baseIso.from(big.rightFunctor.map(out)(tt.from))
      }
  }
  trait Prio2 extends Prio1 {
    implicit def recL[F[_, _], G[_, _], R, FL <: F[_, _], GL <: G[_, _]]
    (implicit baseIso: IsoK2[F, G], bif: Bifunctor[F], big: Bifunctor[G], tt: MapTreeK2[FL, GL]) =
      new MapTreeK2[F[FL, R], G[GL, R]] {
        override def to(in: F[FL, R]): G[GL, R] = baseIso.to(bif.leftFunctor.map(in)(tt.to))
        override def from(out: G[GL, R]): F[FL, R] = baseIso.from(big.leftFunctor.map(out)(tt.from))
      }
  }

  object MapTreeK2 extends Prio2 {
    implicit def recLR[F[_, _], G[_, _], FL <: F[_, _], FR <: F[_, _], GL <: G[_, _], GR <: G[_, _]]
    (implicit baseIso: IsoK2[F, G], bif: Bifunctor[F], big: Bifunctor[G], ttl: MapTreeK2[FL, GL], ttr: MapTreeK2[FR, GR]) =
      new MapTreeK2[F[FL, FR], G[GL, GR]] {
        override def to(in: F[FL, FR]): G[GL, GR] = baseIso.to(bif.bimap(in)(ttl.to, ttr.to))
        override def from(out: G[GL, GR]): F[FL, FR] = baseIso.from(big.bimap(out)(ttl.from, ttr.from))
      }
  }

}
