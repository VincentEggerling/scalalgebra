package scalalgebra.structs

import scalalgebra.ops.FlattenRightOps.FlattenRight
import scalalgebra.utils.Iso

object SemigroupIsoOps {

  trait SemigroupIso[X, Y] extends Iso[X, Y]

  object SemigroupIso {
    implicit def semigroupIsoImpl[X, Y, XY]
    (implicit
     flatX: FlattenRight.Aux[X, XY],
     flatY: FlattenRight.Aux[Y, XY]
    ) = new SemigroupIso[X, Y] {
      override def to(in: X): Y = flatY.inverse(flatX.apply(in))
      override def from(out: Y): X = flatX.inverse(flatY.apply(out))
    }
  }
}
