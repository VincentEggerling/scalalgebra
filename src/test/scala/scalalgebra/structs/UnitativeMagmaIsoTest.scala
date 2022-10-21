package scalalgebra.structs

import scalalgebra.AlgebraStructureOps.unitativeMagmaIso
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.axioms.Tuple2Axioms
import shapeless.test.illTyped


class UnitativeMagmaIsoTest extends AnyFunSuite {


  test("No Axioms") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity

    unitativeMagmaIso[String, String]
    unitativeMagmaIso[Unit, Unit]
    unitativeMagmaIso[Unitt, Unitt]

    // Without the axioms we don't know what the unit is for Tuple2.
    illTyped("""unitativeMagmaIso[(String, Int), (String, Int)]""")
    illTyped("""unitativeMagmaIso[(String, Unit), (String, Unit)]""")

    illTyped(""" unitativeMagmaIso[(String, Unit), String] """)
    illTyped(""" unitativeMagmaIso[(String, Unitt), String] """)
    illTyped(""" unitativeMagmaIso[(Unit, String), String] """)
    illTyped(""" unitativeMagmaIso[(Unitt, String), String] """)
  }

  test("Test tuple2") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraStructureOps._

    assert(unitativeMagmaIso[Unitt, Unitt].to(()) == ())
    assert(unitativeMagmaIso[Unitt, Unitt].from(()) == ())

    assert(unitativeMagmaIso[Unit, Unit].to(()) == ())
    assert(unitativeMagmaIso[Unit, Unit].from(()) == ())

    assert(unitativeMagmaIso[String, String].to("test") == "test")
    assert(unitativeMagmaIso[String, String].from("test") == "test")

    val to1 = ("test", ())
    val from1 = "test"
    assert(unitativeMagmaIso[(String, Unitt), String].to(to1) == from1)
    assert(unitativeMagmaIso[(String, Unitt), String].from(from1) == to1)
    assert(to1.unitativeMagmaIso[String] == from1)

    assert(unitativeMagmaIso[(String, Unit), String].to(("test", ())) == "test")
    assert(unitativeMagmaIso[(String, Unit), String].from("test") == ("test", ()))

    assert(unitativeMagmaIso[String, (String, Unitt)].to("test") == ("test", ()))
    assert(unitativeMagmaIso[String, (String, Unitt)].from(("test", ())) == "test")

    assert(unitativeMagmaIso[String, (String, Unit)].to("test") == ("test", ()))
    assert(unitativeMagmaIso[String, (String, Unit)].from(("test", ())) == "test")

    assert(unitativeMagmaIso[(String, Int), (String, Int)].to(("test", 42)) == ("test", 42))
    assert(unitativeMagmaIso[(String, Int), (String, Int)].from(("test", 42)) == ("test", 42))

    assert(unitativeMagmaIso[(Unitt, Int), (Unitt, Int)].to(((), 42)) == ((), 42))
    assert(unitativeMagmaIso[(Unitt, Int), (Unit, Int)].to(((), 42)) == ((), 42))
    assert(unitativeMagmaIso[(Unit, Int), (Unitt, Int)].from(((), 42)) == ((), 42))
    assert(unitativeMagmaIso[(Unit, Int), (Unit, Int)].from(((), 42)) == ((), 42))

    val to2 = ((), 42)
    val from2 = (42, ())
    assert(unitativeMagmaIso[(Unitt, Int), (Int, Unitt)].to(to2) == from2)
    assert(unitativeMagmaIso[(Unitt, Int), (Int, Unitt)].from(from2) == to2)
    assert(to2.unitativeMagmaIso[(Int, Unitt)] == from2)
    assert(from2.unitativeMagmaIso[(Unitt, Int)] == to2)

    assert(unitativeMagmaIso[(Unitt, (Int, String)), (Int, String)].to(((), (42, "test"))) == (42, "test"))
    assert(unitativeMagmaIso[(Unitt, (Int, String)), (Int, String)].from((42, "test")) == ((), (42, "test")))

    val to5 = ((42, ()), (("test", ()), (90000000L, (0.123, ()))))
    val from5 = (42, (("test", ()), (90000000L, 0.123)))
    val iso5 = unitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Long, Double)))]
    assert(iso5.to(to5) == from5)
    assert(iso5.from(from5) == to5)

    illTyped(""" unitativeMagmaIso[(Long, Int), (String, Int)] """)
    illTyped(""" unitativeMagmaIso[(String, Int), (Unitt, Int)] """)
    illTyped(""" unitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Double, ((String, Unitt), (Long, Int)))] """)
    illTyped(""" unitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Unitt, Double)))] """)
  }


}
