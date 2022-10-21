package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.commutativeMonoidIso
import scalalgebra.axioms.Tuple2Axioms
import shapeless.test.illTyped

class CommutativeMonoidIsoTest extends AnyFunSuite {


  test("No axioms") {
    val iso = commutativeMonoidIso[String, String]
    assert(iso.to("test") == "test")
    assert(iso.from("test") == "test")

    commutativeMonoidIso[Unit, Unit]

    // Without the axioms we don't know what the unit is for Tuple2.
    illTyped("""commutativeMonoidIso[(String, Int), (String, Int)]""")
    illTyped("""commutativeMonoidIso[(String, Unit), (String, Unit)]""")
    illTyped(""" commutativeMonoidIso[((String, Int), Long), ((String, Int), Long)] """)
    illTyped(""" commutativeMonoidIso[String, Unit] """)
    illTyped(""" commutativeMonoidIso[(Int, String), (String, Int)] """)
    illTyped(""" commutativeMonoidIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))] """)

  }

  test("Tuple2") {
    import scalalgebra.AlgebraStructureOps._
    import scalalgebra.axioms.Tuple2Axioms._

    // Associative

    commutativeMonoidIso[Unit, Unit]
    commutativeMonoidIso[(String, Int), (String, Int)]
    commutativeMonoidIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]
    commutativeMonoidIso[((String, Int), Long), ((String, Int), Long)]

    illTyped(""" commutativeMonoidIso[String, Unit] """)

    val to1 = ("test", ((42, 0.123), 9000000L))
    val from1 = (("test", 42), (0.123, 9000000L))
    val iso1 = commutativeMonoidIso[(String, ((Int, Double), Long)), ((String, Int), (Double, Long))]
    assert(iso1.to(to1) == from1)
    assert(iso1.from(from1) == to1)

    // Commutative

    commutativeMonoidIso[Unit, Unit]
    commutativeMonoidIso[(String, Int), (String, Int)]
    commutativeMonoidIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]

    illTyped(""" commutativeMonoidIso[String, Unit] """)
    commutativeMonoidIso[(Int, String), (String, Int)]

    val to6 = (("test", 42), (0.123, 9000000L))
    val from6 = ((0.123, 9000000L), ("test", 42))
    val iso6 = commutativeMonoidIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))]
    assert(iso6.to(to6) == from6)
    assert(iso6.from(from6) == to6)

    assert(to6.commutativeMonoidIso[((Double, Long), (String, Int))] == from6)
    assert(from6.commutativeMonoidIso[((String, Int), (Double, Long))] == to6)


    // Unitative
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    assert(commutativeMonoidIso[Unitt, Unitt].to(()) == ())
    assert(commutativeMonoidIso[Unitt, Unitt].from(()) == ())

    assert(commutativeMonoidIso[Unit, Unit].to(()) == ())
    assert(commutativeMonoidIso[Unit, Unit].from(()) == ())

    assert(commutativeMonoidIso[String, String].to("test") == "test")
    assert(commutativeMonoidIso[String, String].from("test") == "test")

    val to7 = ("test", ())
    val from7 = "test"
    assert(commutativeMonoidIso[(String, Unitt), String].to(to7) == from7)
    assert(commutativeMonoidIso[(String, Unitt), String].from(from7) == to7)
    assert(to7.commutativeMonoidIso[String] == from7)

    assert(commutativeMonoidIso[(String, Unit), String].to(("test", ())) == "test")
    assert(commutativeMonoidIso[(String, Unit), String].from("test") == ("test", ()))

    assert(commutativeMonoidIso[String, (String, Unitt)].to("test") == ("test", ()))
    assert(commutativeMonoidIso[String, (String, Unitt)].from(("test", ())) == "test")

    assert(commutativeMonoidIso[String, (String, Unit)].to("test") == ("test", ()))
    assert(commutativeMonoidIso[String, (String, Unit)].from(("test", ())) == "test")

    assert(commutativeMonoidIso[(String, Int), (String, Int)].to(("test", 42)) == ("test", 42))
    assert(commutativeMonoidIso[(String, Int), (String, Int)].from(("test", 42)) == ("test", 42))

    assert(commutativeMonoidIso[(Unitt, Int), (Unitt, Int)].to(((), 42)) == ((), 42))
    assert(commutativeMonoidIso[(Unitt, Int), (Unit, Int)].to(((), 42)) == ((), 42))
    assert(commutativeMonoidIso[(Unit, Int), (Unitt, Int)].from(((), 42)) == ((), 42))
    assert(commutativeMonoidIso[(Unit, Int), (Unit, Int)].from(((), 42)) == ((), 42))

    val to2 = ((), 42)
    val from2 = (42, ())
    assert(commutativeMonoidIso[(Unitt, Int), (Int, Unitt)].to(to2) == from2)
    assert(commutativeMonoidIso[(Unitt, Int), (Int, Unitt)].from(from2) == to2)
    assert(to2.commutativeMonoidIso[(Int, Unitt)] == from2)
    assert(from2.commutativeMonoidIso[(Unitt, Int)] == to2)

    assert(commutativeMonoidIso[(Unitt, (Int, String)), (Int, String)].to(((), (42, "test"))) == (42, "test"))
    assert(commutativeMonoidIso[(Unitt, (Int, String)), (Int, String)].from((42, "test")) == ((), (42, "test")))

    val to5 = ((42, ()), (("test", ()), (90000000L, (0.123, ()))))
    val from5 = (42, (("test", ()), (90000000L, 0.123)))
    val iso5 = commutativeMonoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Long, Double)))]
    assert(iso5.to(to5) == from5)
    assert(iso5.from(from5) == to5)

    illTyped(""" commutativeMonoidIso[(Long, Int), (String, Int)] """)
    illTyped(""" commutativeMonoidIso[(String, Int), (Unitt, Int)] """)
    illTyped(""" commutativeMonoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Unitt, Double)))] """)


    // Associative Commutative Unitative

    commutativeMonoidIso[((String, Int), (Double, (Long, Float))), ((Double, Int), (Long, (Float, String)))]
    commutativeMonoidIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Double, ((String, Unitt), (Long, Int)))]
    commutativeMonoidIso[((String, Int), ((Unit, (Double, Unit)), (Long, Float))), (((Unit, Double), Int), (Long, (Float, String)))]
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

    type X1 = ((((((Unit, (Unit, (Unit, A))), B), (((C, D), E), (((F, (G, Unit)), H), I))), J), ((Unit, (K, Unit)), (L, M))), (N, O))
    type X2 = (((((((F, (Unit, B)), C), A), ((G, ((L, E), (H, Unit))), ((M, J), (K, D)))), O), N), I)

    commutativeMonoidIso[X1, X2]

  }

}
