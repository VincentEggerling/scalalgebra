package scalalgebra.structs

import scalalgebra.ops.FlattenRightOps.FlattenRight
import scalalgebra.ops.AlignRLBranchesOps.AlignRLBranches
import scalalgebra.ops.ReduceUnitOps.ReduceUnit
import scalalgebra.utils.Iso

object CommutativeMonoidIsoOps {

  trait CommutativeMonoidIso[X, Y] extends Iso[X, Y]

  object CommutativeMonoidIso {
    implicit def commutativeMonoidImpl[X, XU, XUF, Y, YU, YUF]
    (implicit
     reduceX: ReduceUnit.Aux[X, XU],
     reduceY: ReduceUnit.Aux[Y, YU],
     flatX: FlattenRight.Aux[XU, XUF],
     flatY: FlattenRight.Aux[YU, YUF],
     alignXY: AlignRLBranches[XUF, YUF]
    ) = new CommutativeMonoidIso[X, Y] {
      override def to(in: X): Y = reduceY.inverse(flatY.inverse(alignXY.to(flatX.apply(reduceX.apply(in)))))
      override def from(out: Y): X = reduceX.inverse(flatX.inverse(alignXY.from(flatY.apply(reduceY.apply(out)))))
    }
  }
}
