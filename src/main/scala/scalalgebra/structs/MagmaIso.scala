package scalalgebra.structs

import scalalgebra.utils.Iso

object MagmaIsoOps {

  trait MagmaIso[X, Y] extends Iso[X, Y]

  object MagmaIso {
    implicit def magmaIsoImpl[X] = new MagmaIso[X, X] {
      override def to(in: X): X = in
      override def from(out: X): X = out
    }
  }
}
