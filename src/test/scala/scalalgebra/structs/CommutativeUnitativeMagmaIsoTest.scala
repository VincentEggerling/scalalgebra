package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.commutativeUnitativeMagmaIso
import scalalgebra.axioms.Tuple2Axioms
import shapeless.test.illTyped


class CommutativeUnitativeMagmaIsoTest extends AnyFunSuite {


  test("No Axioms") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity

    commutativeUnitativeMagmaIso[String, String]
    commutativeUnitativeMagmaIso[Unit, Unit]
    commutativeUnitativeMagmaIso[Unitt, Unitt]

    // Without the axioms we don't know what the unit is for Tuple2.
    illTyped(""" commutativeUnitativeMagmaIso[(String, Int), (String, Int)]""")
    illTyped(""" commutativeUnitativeMagmaIso[(String, Unit), (String, Unit)]""")

    illTyped(""" commutativeUnitativeMagmaIso[(String, Unit), String] """)
    illTyped(""" commutativeUnitativeMagmaIso[(String, Unitt), String] """)
    illTyped(""" commutativeUnitativeMagmaIso[(Unit, String), String] """)
    illTyped(""" commutativeUnitativeMagmaIso[(Unitt, String), String] """)

    illTyped(""" commutativeUnitativeMagmaIso[(String, Int), (Int, String)] """)
  }

  test("Test tuple2") {
    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    import scalalgebra.AlgebraStructureOps._
    import scalalgebra.axioms.Tuple2Axioms._

    assert(commutativeUnitativeMagmaIso[Unitt, Unitt].to(()) == ())
    assert(commutativeUnitativeMagmaIso[Unitt, Unitt].from(()) == ())

    assert(commutativeUnitativeMagmaIso[Unit, Unit].to(()) == ())
    assert(commutativeUnitativeMagmaIso[Unit, Unit].from(()) == ())

    assert(commutativeUnitativeMagmaIso[String, String].to("test") == "test")
    assert(commutativeUnitativeMagmaIso[String, String].from("test") == "test")

    val to1 = ("test", ())
    val from1 = "test"
    assert(commutativeUnitativeMagmaIso[(String, Unitt), String].to(to1) == from1)
    assert(commutativeUnitativeMagmaIso[(String, Unitt), String].from(from1) == to1)
    assert(to1.unitativeMagmaIso[String] == from1)

    assert(commutativeUnitativeMagmaIso[(String, Unit), String].to(("test", ())) == "test")
    assert(commutativeUnitativeMagmaIso[(String, Unit), String].from("test") == ("test", ()))

    assert(commutativeUnitativeMagmaIso[String, (String, Unitt)].to("test") == ("test", ()))
    assert(commutativeUnitativeMagmaIso[String, (String, Unitt)].from(("test", ())) == "test")

    assert(commutativeUnitativeMagmaIso[String, (String, Unit)].to("test") == ("test", ()))
    assert(commutativeUnitativeMagmaIso[String, (String, Unit)].from(("test", ())) == "test")

    assert(commutativeUnitativeMagmaIso[(String, Int), (String, Int)].to(("test", 42)) == ("test", 42))
    assert(commutativeUnitativeMagmaIso[(String, Int), (String, Int)].from(("test", 42)) == ("test", 42))

    assert(commutativeUnitativeMagmaIso[(Unitt, Int), (Unitt, Int)].to(((), 42)) == ((), 42))
    assert(commutativeUnitativeMagmaIso[(Unitt, Int), (Unit, Int)].to(((), 42)) == ((), 42))
    assert(commutativeUnitativeMagmaIso[(Unit, Int), (Unitt, Int)].from(((), 42)) == ((), 42))
    assert(commutativeUnitativeMagmaIso[(Unit, Int), (Unit, Int)].from(((), 42)) == ((), 42))

    val to2 = ((), 42)
    val from2 = (42, ())
    assert(commutativeUnitativeMagmaIso[(Unitt, Int), (Int, Unitt)].to(to2) == from2)
    assert(commutativeUnitativeMagmaIso[(Unitt, Int), (Int, Unitt)].from(from2) == to2)
    assert(to2.commutativeUnitativeMagmaIso[(Int, Unitt)] == from2)
    assert(from2.commutativeUnitativeMagmaIso[(Unitt, Int)] == to2)

    assert(commutativeUnitativeMagmaIso[(Unitt, (Int, String)), (Int, String)].to(((), (42, "test"))) == (42, "test"))
    assert(commutativeUnitativeMagmaIso[(Unitt, (Int, String)), (Int, String)].from((42, "test")) == ((), (42, "test")))

    val to5 = ((42, ()), (("test", ()), (90000000L, (0.123, ()))))
    val from5 = (42, (("test", ()), (90000000L, 0.123)))
    val iso5 = commutativeUnitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Long, Double)))]
    assert(iso5.to(to5) == from5)
    assert(iso5.from(from5) == to5)

    illTyped(""" commutativeUnitativeMagmaIso[(Long, Int), (String, Int)] """)
    illTyped(""" commutativeUnitativeMagmaIso[(String, Int), (Unitt, Int)] """)
    illTyped(""" commutativeUnitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Double, ((String, Unitt), (Long, Int)))] """)
    illTyped(""" commutativeUnitativeMagmaIso[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt)))), (Int, ((String, Unitt), (Unitt, Double)))] """)


    // Commutative

    commutativeUnitativeMagmaIso[Unit, Unit]
    commutativeUnitativeMagmaIso[(String, Int), (String, Int)]
    commutativeUnitativeMagmaIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]

    illTyped(""" commutativeUnitativeMagmaIso[String, Unit] """)
    commutativeUnitativeMagmaIso[(Int, String), (String, Int)]

    val to6 = (("test", 42), (0.123, 9000000L))
    val from6 = ((0.123, 9000000L), ("test", 42))
    val iso6 = commutativeUnitativeMagmaIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))]
    assert(iso6.to(to6) == from6)
    assert(iso6.from(from6) == to6)

    assert(to6.commutativeUnitativeMagmaIso[((Double, Long), (String, Int))] == from6)
    assert(from6.commutativeUnitativeMagmaIso[((String, Int), (Double, Long))] == to6)


    // CommutativeUnitative

    commutativeUnitativeMagmaIso[((String, Unit), Int), (Int, (Unit, ((Unit, String), Unit)))]
  }

  test("compilation time") {
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


    type X1 = (((((A, (Unit, (Unit, B))), (C, D)), E), (Unit, F)), ((G, H), (I, (((J, Unit), K), ((L, M), N)))))

    commutativeUnitativeMagmaIso[X1, (((I, ((K, J), (N, (L, M)))), (G, H)), (F, (((A, B), (D, C)), E)))]
    commutativeUnitativeMagmaIso[X1,
      (((I, ((K, (Unit, J)), (N, (L, M)))), ((G, Unit), H)), (F, ((((Unit, (Unit, A)), B), (D, C)), E)))
    ]

  }


}
