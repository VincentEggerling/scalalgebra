package scalalgebra.structs

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraStructureOps.commutativeSemigroupIso
import shapeless.test.illTyped

class CommutativeSemigroupIsoTest extends AnyFunSuite {


  test("No axioms") {
    val iso = commutativeSemigroupIso[String, String]
    assert(iso.to("test") == "test")
    assert(iso.from("test") == "test")

    commutativeSemigroupIso[Unit, Unit]
    commutativeSemigroupIso[(String, Int), (String, Int)]
    commutativeSemigroupIso[(String, (Int, Long)), (String, (Int, Long))]
    commutativeSemigroupIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]


    illTyped(""" commutativeSemigroupIso[((String, Int), Long), ((String, Int), Long)] """) // FlattenRight requires Associative to work correctly.
    illTyped(""" commutativeSemigroupIso[String, Unit] """)
    illTyped(""" commutativeSemigroupIso[(Int, String), (String, Int)] """)
    illTyped(""" commutativeSemigroupIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))] """)

  }

  test("Tuple2") {
    import scalalgebra.AlgebraStructureOps._
    import scalalgebra.axioms.Tuple2Axioms._

    // Associative

    commutativeSemigroupIso[Unit, Unit]
    commutativeSemigroupIso[(String, Int), (String, Int)]
    commutativeSemigroupIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]
    commutativeSemigroupIso[((String, Int), Long), ((String, Int), Long)]

    illTyped(""" commutativeSemigroupIso[String, Unit] """)
    illTyped(""" commutativeSemigroupIso[(String, Unit), String] """)

    val to1 = ("test", ((42, 0.123), 9000000L))
    val from1 = (("test", 42), (0.123, 9000000L))
    val iso1 = commutativeSemigroupIso[(String, ((Int, Double), Long)), ((String, Int), (Double, Long))]
    assert(iso1.to(to1) == from1)
    assert(iso1.from(from1) == to1)

    // Commutative

    commutativeSemigroupIso[Unit, Unit]
    commutativeSemigroupIso[(String, Int), (String, Int)]
    commutativeSemigroupIso[((String, Int), (Double, Long)), ((String, Int), (Double, Long))]

    illTyped(""" commutativeSemigroupIso[String, Unit] """)
    commutativeSemigroupIso[(Int, String), (String, Int)]

    val to6 = (("test", 42), (0.123, 9000000L))
    val from6 = ((0.123, 9000000L), ("test", 42))
    val iso6 = commutativeSemigroupIso[((String, Int), (Double, Long)), ((Double, Long), (String, Int))]
    assert(iso6.to(to6) == from6)
    assert(iso6.from(from6) == to6)

    assert(to6.commutativeSemigroupIso[((Double, Long), (String, Int))] == from6)
    assert(from6.commutativeSemigroupIso[((String, Int), (Double, Long))] == to6)


    // Associative Commutative

    commutativeSemigroupIso[((String, Int), (Double, (Long, Float))), ((Double, Int), (Long, (Float, String)))]
    illTyped(""" commutativeSemigroupIso[((String, Int), (Double, (Long, Float))), (((Unit, Double), Int), (Long, (Float, String)))] """)
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
    type X2 = (((((((F, B), C), A), ((G, ((L, E), H)), ((M, J), (K, D)))), O), N), I)

    commutativeSemigroupIso[X1, X2]

  }

}
