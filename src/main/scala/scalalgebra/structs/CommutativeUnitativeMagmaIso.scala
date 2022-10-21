package scalalgebra.structs

import scalalgebra.ops.CommuteTreeOps.CommuteTree
import scalalgebra.ops.ReduceUnitOps.ReduceUnit
import scalalgebra.utils.Iso

object CommutativeUnitativeMagmaIsoOps {

  trait CommutativeUnitativeMagmaIso[X, Y] extends Iso[X, Y]

  implicit def commutativeUnitativeMagmaIsoImpl[X, Y, XU, YU]
  (implicit
   reduceX: ReduceUnit.Aux[X, XU],
   reduceY: ReduceUnit.Aux[Y, YU],
   commTree: CommuteTree[XU, YU]
  ) = new CommutativeUnitativeMagmaIso[X, Y] {
    override def to(in: X): Y = reduceY.inverse(commTree.to(reduceX.apply(in)))
    override def from(out: Y): X = reduceX.inverse(commTree.from(reduceY.apply(out)))
  }
}
