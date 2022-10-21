package scalalgebra.structs

import scalalgebra.ops.FlattenRightOps.FlattenRight
import scalalgebra.ops.ReduceUnitOps.ReduceUnit
import scalalgebra.utils.Iso

object MonoidIsoOps {

  trait MonoidIso[X, Y] extends Iso[X, Y]

  object MonoidIso {
    implicit def monoidIsoImpl[X, XU, Y, YU, XY]
    (implicit
     reduceX: ReduceUnit.Aux[X, XU],
     reduceY: ReduceUnit.Aux[Y, YU],
     flatX: FlattenRight.Aux[XU, XY],
     flatY: FlattenRight.Aux[YU, XY]
    ) = new MonoidIso[X, Y] {
      override def to(in: X): Y = reduceY.inverse(flatY.inverse(flatX.apply(reduceX.apply(in))))
      override def from(out: Y): X = reduceX.inverse(flatX.inverse(flatY.apply(reduceY.apply(out))))
    }
  }
}
