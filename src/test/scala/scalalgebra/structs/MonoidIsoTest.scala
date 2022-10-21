package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.monoidIso
import scalalgebra.axioms.Tuple2Axioms
import shapeless.test.illTyped


class MonoidIsoTest extends AnyFunSuite {


  test("No Axioms") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity

    monoidIso[String, String]
    monoidIso[Unit, Unit]
    monoidIso[Unitt, Unitt]


    // Without the axioms we don't know what the unit is for Tuple2.
    illTyped("""monoidIso[(String, Int), (String, Int)]""")
    illTyped("""monoidIso[(String, Unit), (String, Unit)]""")

    illTyped(""" monoidIso[(String, Unit), String] """)
    illTyped(""" monoidIso[(String, Unitt), String] """)
    illTyped(""" monoidIso[(Unit, String), String] """)
    illTyped(""" monoidIso[(Unitt, String), String] """)
  }

  test("Test tuple2") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    import scalalgebra.AlgebraStructureOps._
    import scalalgebra.axioms.Tuple2Axioms._

    // Unitative

    assert(monoidIso[Unitt, Unitt].to(()) == ())
    assert(monoidIso[Unitt, Unitt].from(()) == ())

    assert(monoidIso[Unit, Unit].to(()) == ())
    assert(monoidIso[Unit, Unit].from(()) == ())

    assert(monoidIso[String, String].to("test") == "test")
    assert(monoidIso[String, String].from("test") == "test")

    val to1 = ("test", ())
    val from1 = "test"
    assert(monoidIso[(String, Unitt), String].to(to1) == from1)
    assert(monoidIso[(String, Unitt), String].from(from1) == to1)
    assert(to1.monoidIso[String] == from1)

    assert(monoidIso[(String, Unit), String].to(("test", ())) == "test")
    assert(monoidIso[(String, Unit), String].from("test") == ("test", ()))

    assert(monoidIso[String, (String, Unitt)].to("test") == ("test", ()))
    assert(monoidIso[String, (String, Unitt)].from(("test", ())) == "test")

    assert(monoidIso[String, (String, Unit)].to("test") == ("test", ()))
    assert(monoidIso[String, (String, Unit)].from(("test", ())) == "test")

    assert(monoidIso[(String, Int), (String, Int)].to(("test", 42)) == ("test", 42))
    assert(monoidIso[(String, Int), (String, Int)].from(("test", 42)) == ("test", 42))

    assert(monoidIso[(Unitt, Int), (Unitt, Int)].to(((), 42)) == ((), 42))
    assert(monoidIso[(Unitt, Int), (Unit, Int)].to(((), 42)) == ((), 42))
    assert(monoidIso[(Unit, Int), (Unitt, Int)].from(((), 42)) == ((), 42))
    assert(monoidIso[(Unit, Int), (Unit, Int)].from(((), 42)) == ((), 42))

    val to2 = ((), 42)
    val from2 = (42, ())
    assert(monoidIso[(Unitt, Int), (Int, Unitt)].to(to2) == from2)
    assert(monoidIso[(Unitt, Int), (Int, Unitt)].from(from2) == to2)
    assert(to2.monoidIso[(Int, Unitt)] == from2)
    assert(from2.monoidIso[(Unitt, Int)] == to2)

    assert(monoidIso[(Unitt, (Int, String)), (Int, String)].to(((), (42, "test"))) == (42, "test"))
    assert(monoidIso[(Unitt, (Int, String)), (Int, String)].from((42, "test")) == ((), (42, "test")))

    val to5 = ((42, ()), (("test", ()), (90000000L, (0.123, ()))))
    val from5 = (42, (("test", ()), (90000000L, 0.123)))
    val iso5 = monoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Long, Double)))]
    assert(iso5.to(to5) == from5)
    assert(iso5.from(from5) == to5)

    illTyped(""" monoidIso[(Long, Int), (String, Int)] """)
    illTyped(""" monoidIso[(String, Int), (Unitt, Int)] """)
    illTyped(""" monoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Double, ((String, Unitt), (Long, Int)))] """)
    illTyped(""" monoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Unitt, Double)))] """)

    // Associative

    monoidIso[(String, (Int, Double)), (String, (Int, Double))]
    monoidIso[((String, Int), Double), (String, (Int, Double))]
    monoidIso[((String, Int), Double), ((String, Int), Double)]
    monoidIso[(String, (Int, Double)), ((String, Int), Double)]

    illTyped(""" monoidIso[(String, (Int, Double)), ((String, Double), Int)] """)
    illTyped(""" monoidIso[((Double, Int), String), (String, (Int, Double))] """)


    // Monoid

    monoidIso[(((String, Unit), Double), Long), (String, ((Double, Long), Unit))]
    monoidIso[(((String, Unit), Double), Long), ((String, (Unit, Unit)), ((Double, Long), Unit))]

    illTyped(""" monoidIso[(((Double, Unit), String), Long), (String, ((Double, Long), Unit))] """)
  }

  test("Compile time") {
    import scalalgebra.axioms.Tuple2Axioms._

    trait A
    trait B
    trait C
    trait D
    trait E
    trait F
    trait G
    trait H
    trait I
    trait J
    trait K
    trait L
    trait M
    trait N
    trait O

    type X1 = (((((A, B), (((C, D), E), (((F, G), H), I))), J), (K, (L, M))), (N, O))
    type X2 = (((((((A, B), C), D), ((E, ((F, G), H)), ((I, J), (K, L)))), M), N), O)

    monoidIso[X1, X2]

    type Y1 = (((((A, B), (((C, (D, Unit)), E), (((F, G), (Unit, H)), I))), J), (K, (((Unit, L), Unit), M))), (N, O))
    type Y2 = ((((((((Unit, A), B), ((Unit, C), Unit)), D), ((E, ((F, G), H)), ((I, J), (K, L)))), M), N), O)

    monoidIso[Y1, Y2]
  }

}
