package scalalgebra.structs

import scalalgebra.ops.ReduceUnitOps.ReduceUnit
import scalalgebra.utils.Iso

object UnitativeMagmaIsoOps {

  trait UnitativeMagmaIso[X, Y] extends Iso[X, Y]

  implicit def unitativeMagmaIsoImpl[X, Y, XY]
  (implicit
   reduceX: ReduceUnit.Aux[X, XY],
   reduceY: ReduceUnit.Aux[Y, XY],
  ) = new UnitativeMagmaIso[X, Y] {
    override def to(in: X): Y = reduceY.inverse(reduceX.apply(in))
    override def from(out: Y): X = reduceX.inverse(reduceY.apply(out))
  }

}
