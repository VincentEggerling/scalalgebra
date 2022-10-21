package scalalgebra.ops

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.axioms.Tuple2Axioms
import scalalgebra.ops.IsNotUnitOps.IsNotUnit
import scalalgebra.testutils.ForceImport.forceUnitativeImplicitImport
import shapeless.test.illTyped

class NotUnitTest extends AnyFunSuite {
  test("test without axioms") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    implicitly[IsNotUnit[Tuple2, Int]]
    implicitly[IsNotUnit[Tuple2, Unitt]]
    implicitly[IsNotUnit[Tuple2, Unit]]
  }

  test("test with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    forceUnitativeImplicitImport

    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    implicitly[IsNotUnit[Tuple2, Int]]
    illTyped(""" implicitly[IsNotUnit[Tuple2, Unitt]] """)
    illTyped(""" implicitly[IsNotUnit[Tuple2, Unit]] """)
  }

}
