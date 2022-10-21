package scalalgebra.structs

import scalalgebra.ops.CommuteTreeOps.CommuteTree
import scalalgebra.utils.Iso

object CommutativeMagmaIsoOps {

  trait CommutativeMagmaIso[X, Y] extends Iso[X, Y]

  object CommutativeMagmaIso {
    implicit def commutativeMagmaIsoImpl[X, Y]
    (implicit
     commTree: CommuteTree[X, Y]
    ) = new CommutativeMagmaIso[X, Y] {
      override def to(in: X): Y = commTree.to(in)
      override def from(out: Y): X = commTree.from(out)
    }
  }
}
