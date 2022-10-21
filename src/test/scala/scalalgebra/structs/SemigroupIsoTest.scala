package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.semigroupIso
import shapeless.test.illTyped

class SemigroupIsoTest extends AnyFunSuite {


  test("No axioms") {
    val iso = semigroupIso[String, String]
    assert(iso.to("test") == "test")
    assert(iso.from("test") == "test")

    semigroupIso[Unit, Unit]
    semigroupIso[(String, Int), (String, Int)]
    semigroupIso[(String, (Int, Long)), (String, (Int, Long))]
    semigroupIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]


    illTyped(""" semigroupIso[((String, Int), Long), ((String, Int), Long)] """) // FlattenRight requires Associative to work correctly.
    illTyped(""" semigroupIso[String, Unit] """)
    illTyped(""" semigroupIso[(Int, String), (String, Int)] """)
    illTyped(""" semigroupIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))] """)

  }

  test("Tuple2") {
    import scalalgebra.AlgebraStructureOps._
    import scalalgebra.axioms.Tuple2Axioms._

    semigroupIso[Unit, Unit]
    semigroupIso[(String, Int), (String, Int)]
    semigroupIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]
    semigroupIso[((String, Int), Long), ((String, Int), Long)]

    illTyped(""" semigroupIso[String, Unit] """)
    illTyped(""" semigroupIso[(Int, String), (String, Int)] """)
    illTyped(""" semigroupIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))] """)
    illTyped(""" semigroupIso[(String, Unit), String] """)

    val to1 = ("test", ((42, 0.123), 9000000L))
    val from1 = (("test", 42), (0.123, 9000000L))
    val iso1 = semigroupIso[(String, ((Int, Double), Long)), ((String, Int), (Double, Long))]
    assert(iso1.to(to1) == from1)
    assert(iso1.from(from1) == to1)

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

    semigroupIso[X1, X2]

  }

}
