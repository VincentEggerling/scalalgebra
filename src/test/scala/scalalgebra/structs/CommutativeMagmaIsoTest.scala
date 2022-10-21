package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.commutativeMagmaIso
import shapeless.test.illTyped

class CommutativeMagmaIsoTest extends AnyFunSuite {

  test("No axioms") {
    val iso = commutativeMagmaIso[String, String]
    assert(iso.to("test") == "test")
    assert(iso.from("test") == "test")

    commutativeMagmaIso[Unit, Unit]
    commutativeMagmaIso[(String, Int), (String, Int)]
    commutativeMagmaIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]

    illTyped(""" commutativeMagmaIso[String, Unit] """)
    illTyped(""" commutativeMagmaIso[(Int, String), (String, Int)] """)
    illTyped(""" commutativeMagmaIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))] """)

  }

  test("Tuple2") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraStructureOps._

    commutativeMagmaIso[Unit, Unit]
    commutativeMagmaIso[(String, Int), (String, Int)]
    commutativeMagmaIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]

    illTyped(""" commutativeMagmaIso[String, Unit] """)
    commutativeMagmaIso[(Int, String), (String, Int)]

    val to1 = (("test", 42), (0.123, 9000000L))
    val from1 = ((0.123, 9000000L), ("test", 42))
    val iso1 = commutativeMagmaIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))]
    assert(iso1.to(to1) == from1)
    assert(iso1.from(from1) == to1)

    assert(to1.commutativeMagmaIso[((Double, Long), (String, Int))] == from1)
    assert(from1.commutativeMagmaIso[((String, Int), (Double, Long))] == to1)

  }

}
