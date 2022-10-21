package scalalgebra.structs

import scalalgebra.structs.CommutativeMagmaIsoOps.CommutativeMagmaIso
import scalalgebra.structs.CommutativeMonoidIsoOps.CommutativeMonoidIso
import scalalgebra.structs.CommutativeSemigroupIsoOps.CommutativeSemigroupIso
import scalalgebra.structs.CommutativeUnitativeMagmaIsoOps.CommutativeUnitativeMagmaIso
import scalalgebra.structs.MagmaIsoOps.MagmaIso
import scalalgebra.structs.MonoidIsoOps.MonoidIso
import scalalgebra.structs.SemigroupIsoOps.SemigroupIso
import scalalgebra.structs.UnitativeMagmaIsoOps.UnitativeMagmaIso
import scalalgebra.utils.Iso

object GenericIsoOps {

  trait GenericIso[X, Y] extends Iso[X, Y]

  // All Prio are needed because sometimes several can apply which lead to ambiguous implicit.
  trait Prio0 {
    implicit def genericCommutativeMonoid[X, Y](implicit e: CommutativeMonoidIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio1 extends Prio0 {
    implicit def genericMonoid[X, Y](implicit e: MonoidIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio2 extends Prio1 {
    implicit def genericCommutativeSemigroup[X, Y](implicit e: CommutativeSemigroupIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio3 extends Prio2 {
    implicit def genericCommutativeUnitativeMagma[X, Y](implicit e: CommutativeUnitativeMagmaIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio4 extends Prio3 {
    implicit def genericSemigroup[X, Y](implicit e: SemigroupIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio5 extends Prio4 {
    implicit def genericUnitativeMagma[X, Y](implicit e: UnitativeMagmaIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  trait Prio6 extends Prio5 {
    implicit def genericCommutativeMagma[X, Y](implicit e: CommutativeMagmaIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }
  object GenericIso extends Prio6 {
    implicit def genericMagma[X, Y](implicit e: MagmaIso[X, Y]) = new GenericIso[X, Y] {
      override def to(in: X): Y = e.to(in)
      override def from(out: Y): X = e.from(out)
    }
  }

}
