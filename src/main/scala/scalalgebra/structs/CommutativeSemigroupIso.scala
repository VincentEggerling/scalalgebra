package scalalgebra.structs

import scalalgebra.ops.AlignRLBranchesOps.AlignRLBranches
import scalalgebra.ops.FlattenRightOps.FlattenRight
import scalalgebra.utils.Iso

object CommutativeSemigroupIsoOps {

  trait CommutativeSemigroupIso[X, Y] extends Iso[X, Y]

  object CommutativeSemigroupIso {
    implicit def commutativeSemigroupIsoImpl[X, Y, XF, YF]
    (implicit
     flatX: FlattenRight.Aux[X, XF],
     flatY: FlattenRight.Aux[Y, YF],
     alignXY: AlignRLBranches[XF, YF]
    ) = new CommutativeSemigroupIso[X, Y] {
      override def to(in: X): Y = flatY.inverse(alignXY.to(flatX.apply(in)))
      override def from(out: Y): X = flatX.inverse(alignXY.from(flatY.apply(out)))
    }
  }
}
