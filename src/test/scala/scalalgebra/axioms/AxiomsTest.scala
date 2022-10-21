package scalalgebra.axioms

import scalalgebra.axioms.AlgebraStructureAxioms.{CommutativeMagma, CommutativeMonoid, CommutativeSemigroup, CommutativeUnitativeMagma, Magma, Monoid, Semigroup, UnitativeMagma}
import org.scalatest.funsuite.AnyFunSuite

class AxiomsTest extends AnyFunSuite {

  test("Hierarchy") {
    implicitly[UnitativeMagma[Tuple2] <:< Magma[Tuple2]]
    implicitly[Semigroup[Tuple2] <:< Magma[Tuple2]]
    implicitly[CommutativeMagma[Tuple2] <:< Magma[Tuple2]]

    implicitly[Monoid[Tuple2] <:< UnitativeMagma[Tuple2]]
    implicitly[Monoid[Tuple2] <:< Semigroup[Tuple2]]
    implicitly[CommutativeUnitativeMagma[Tuple2] <:< CommutativeMagma[Tuple2]]
    implicitly[CommutativeUnitativeMagma[Tuple2] <:< UnitativeMagma[Tuple2]]
    implicitly[CommutativeSemigroup[Tuple2] <:< Semigroup[Tuple2]]
    implicitly[CommutativeSemigroup[Tuple2] <:< CommutativeMagma[Tuple2]]


    implicitly[CommutativeMonoid[Tuple2] <:< Monoid[Tuple2]]
    implicitly[CommutativeMonoid[Tuple2] <:< CommutativeUnitativeMagma[Tuple2]]
    implicitly[CommutativeMonoid[Tuple2] <:< CommutativeSemigroup[Tuple2]]

    assert(true)
  }
}