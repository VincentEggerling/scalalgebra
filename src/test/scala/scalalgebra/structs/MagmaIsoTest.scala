package scalalgebra.structs

import scalalgebra.AlgebraStructureOps.magmaIso
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.testutils.ForceImport.forceAssociativeImplicitImport
import shapeless.test.illTyped

class MagmaIsoTest extends AnyFunSuite {

  test("Trivial") {
    assert(magmaIso[String, String].to("test") == "test")
    assert(magmaIso[String, String].from("test") == "test")

    assert(magmaIso[Unit, Unit].to(()) == ())
    assert(magmaIso[Unit, Unit].from(()) == ())

    illTyped(""" magmaIso[Unit, String] """)
    illTyped(""" magmaIso[String, Unit] """)
  }

  test("Tuple2") {
    import scalalgebra.axioms.Tuple2Axioms._
    forceAssociativeImplicitImport
    import scalalgebra.AlgebraStructureOps._

    val fromTo1 = ("test", 42)
    val iso1 = magmaIso[(String, Int), (String, Int)]
    assert(iso1.to(fromTo1) == fromTo1)
    assert(iso1.from(fromTo1) == fromTo1)
    assert(fromTo1.magmaIso[(String, Int)] == fromTo1)

    illTyped(""" magmaIso[(String, Int), (Int, String)] """)
    illTyped(""" magmaIso[(String, (Int, Double)), ((String, Int), Double)] """)
    illTyped(""" magmaIso[(String, Unit), String] """)
  }

}
