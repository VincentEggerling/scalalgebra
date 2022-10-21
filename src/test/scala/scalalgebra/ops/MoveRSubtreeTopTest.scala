package scalalgebra.ops

import scalalgebra.ops.MoveRSubtreeTopOps.MoveRSubtreeTop
import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped

class MoveRSubtreeTopTest extends AnyFunSuite {

  trait Rest[X]
  def rest[F[_, _], X, SubT](implicit e: MoveRSubtreeTop[F, X, SubT]): Rest[e.RestX] = new Rest[e.RestX] {}

  test("No axioms") {
    import scalalgebra.AlgebraTreeOps._
    ("test", 42).alignRLBranch[(String, Int)] // Ensures that the import statement is used and doesn't get optimized out.

    illTyped(""" moveRSubtreeTop[Tuple2, (String, Int), String] """)
    illTyped(""" moveRSubtreeTop[Tuple2, (String, Int), Long] """)
    illTyped(""" moveRSubtreeTop[Tuple2, (String, Int), Int] """)

    val x = ("test", 12)
    illTyped(""" x.moveRSubtreeTop[Int] """)
    illTyped(""" x.moveRSubtreeTop[String] """)
  }

  test("test") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val r1 = moveRSubtreeTop[Tuple2, (String, Int), Int]
    assert(r1.apply("test", 42) == (42, "test"))
    assert(r1.inverse(42, "test") == ("test", 42))

    illTyped(""" moveRSubtreeTop[Tuple2, (String, Int), String] """) // Cannot search in left main branch

    val r2 = moveRSubtreeTop[Tuple2, ((String, Double), Int), Int]
    val to2 = (("test", 0.123), 42)
    val from2 = (42, ("test", 0.123))
    assert(r2.apply(to2) == from2)
    assert(r2.inverse(from2) == to2)
    assert(to2.moveRSubtreeTop[Int] == from2)

    illTyped(""" moveRSubtreeTop[Tuple2, ((String, Double), Int), Double] """) // Cannot search in the left main branch
    illTyped(""" moveRSubtreeTop[Tuple2, (String, Int), (String, Int)] """) // Cannot move the entire tree

    rest[Tuple2, ((String, Double), (Int, Long)), Long]: Rest[((String, Double), Int)]
    illTyped(""" rest[Tuple2, ((String, Double), (Int, Long)), Long] : Rest[((Double, String),Int)] """)
    illTyped(""" rest[Tuple2, ((String, Double), (Int, Long)), Long] : Rest[((String, Int), Double)] """)
    illTyped(""" rest[Tuple2, ((String, Double), (Int, Long)), Long] : Rest[((String, Double), Long)] """) // Cannot search on an RL branch

    rest[Tuple2, ((String, Double), (Int, Long)), (Int, Long)]: Rest[(String, Double)]
    illTyped(""" rest[Tuple2, ((String, Double), (Int, Long)), (Int, Long)] : Rest[(Double, String)] """) // Wrong rest
    illTyped(""" rest[Tuple2, ((String, Double), (Int, Long)), (Long, Int)] """) // Subtree doesn't exist

    rest[Tuple2, ((String, (Double, Float)), ((Int, Boolean), Long)), Long]: Rest[((String, (Double, Float)), (Int, Boolean))]
    rest[Tuple2, ((String, (Double, Float)), ((Int, Boolean), Long)), ((Int, Boolean), Long)]: Rest[(String, (Double, Float))]
    illTyped(""" rest[Tuple2, ((String, (Double, Float)), ((Int, Boolean), Long)), Long] : Rest[((String, (Double, Float)), (Boolean, Int))] """)

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
    trait P
    trait Q
    trait R
    trait S
    trait T
    trait U
    trait V
    trait W
    trait X
    trait Y
    trait Z
    trait AA
    trait AB
    trait AC
    trait AD
    trait AE
    trait AF
    trait AG
    trait AH
    trait AI
    trait AJ
    trait AK

    type Y1_1 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, ((T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK))))))))))))))))), S)))))))))))))))))))
    type Y1_2 = ((T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK))))))))))))))))), S)
    type Y1_3 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, R)))))))))))))))))

    rest[Tuple2, Y1_1, Y1_2]: Rest[Y1_3]
  }

}
