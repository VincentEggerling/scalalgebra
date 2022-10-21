package scalalgebra.ops

import scalalgebra.AlgebraTreeOps.moveRLBranchTop
import scalalgebra.ops.MoveRLBranchTopOps.MoveRLBranchTop
import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped

class MoveRLBranchTopTest extends AnyFunSuite {

  trait Rest[X]
  def rest[F[_, _], X, RLB](implicit e: MoveRLBranchTop[F, X, RLB]): Rest[e.XRest] = new Rest[e.XRest] {}

  test("Base no axiom") {
    import scalalgebra.AlgebraTreeOps._

    val r1: MoveRLBranchTop.Aux[Tuple2, (String, Int), String, Int] = moveRLBranchTop[Tuple2, (String, Int), String]
    val r1ToFrom = ("test", 42)
    assert(r1.apply(r1ToFrom) == r1ToFrom)
    assert(r1.inverse(r1ToFrom) == r1ToFrom)
    assert(r1.moveRLBranchTop[String] == r1)

    illTyped(""" rest[Tuple2, (String, Int), Int] """) // True, but needs commutative
    illTyped(""" rest[Tuple2, String, String] """) // Rest would need to be empty
    illTyped(""" rest[Tuple2, String, Int] """) // Int is not an RL branch in String

    val r2 = moveRLBranchTop[Tuple2, (String, (Int, Long)), String]
    val r2ToFrom = ("test", (42, 900000000L))
    assert(r2.apply(r2ToFrom) == r2ToFrom)
    assert(r2.inverse(r2ToFrom) == r2ToFrom)
    assert(r2ToFrom.moveRLBranchTop[String] == r2ToFrom)

    val r3 = moveRLBranchTop[Tuple2, ((String, Int), (Double, Long)), (String, Int)]
    val r3ToFrom = (("test", 42), (0.123, 90000000L))
    assert(r3.apply(r3ToFrom) == r3ToFrom)
    assert(r3.inverse(r3ToFrom) == r3ToFrom)
    assert(r3ToFrom.moveRLBranchTop[(String, Int)] == r3ToFrom)

    illTyped(""" moveRLBranchTop[Tuple2, (String, (Int, Long)), Int] """)
    illTyped(""" moveRLBranchTop[Tuple2, (String, (Int, Long)), Long] """)
    illTyped(""" r2ToFrom.moveRLBranchTop[Int] """)
    illTyped(""" r2ToFrom.moveRLBranchTop[Long] """)
  }


  test("Tuple2") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val m1 = moveRLBranchTop[Tuple2, (String, Int), String]
    val toFrom1 = ("test", 42)
    assert(m1.apply(toFrom1) == toFrom1)
    assert(m1.inverse(toFrom1) == toFrom1)
    assert(toFrom1.moveRLBranchTop[String] == toFrom1)

    illTyped(""" rest[Tuple2, String, String] """) // Rest would be empty
    illTyped(""" rest[Tuple2, String, Int] """) // Int is not an RL branch in String
    illTyped(""" rest[Tuple2, (String, Int), Int] """) // Int is a leaf


    illTyped(""" rest[Tuple2, (String, Int), (String, Int)] """) // Rest would need to be empty
    illTyped(""" rest[Tuple2, (String, Int), (Int, String)] """) // RL branch doesn't exist


    val m3 = moveRLBranchTop[Tuple2, (String, (Int, Long)), String]
    val toFrom3 = ("test", (42, 900000000L))
    assert(m3.apply(toFrom3) == toFrom3)
    assert(m3.inverse(toFrom3) == toFrom3)

    illTyped(""" rest[Tuple2, (String, (Int, Long)), String] : Rest[Int] """)
    illTyped(""" rest[Tuple2, (String, (Int, Long)), String] : Rest[Long] """)

    val m4 = moveRLBranchTop[Tuple2, (String, (Int, Long)), Int]
    val to4 = ("test", (42, 900000000L))
    val from4 = (42, ("test", 900000000L))
    assert(m4.apply(to4) == from4)
    assert(m4.inverse(from4) == to4)


    illTyped(""" rest[Tuple2, (String, (Int, Long)), Int] : Rest[(Long, String)] """)
    illTyped(""" rest[Tuple2, (String, (Int, Long)), Int] : Rest[Long] """)

    illTyped(""" rest[Tuple2, (String, (Int, Long)), Long] """) // Cannot remove leaf
    illTyped(""" rest[Tuple2, ((String, Int), Long), Long] """) // Cannot remove leaf

    illTyped(""" rest[Tuple2, ((String, Int), Long), Int] """) // Int is not an RL branch

    val m7 = moveRLBranchTop[Tuple2, ((String, Int), Long), (String, Int)]
    val toFrom7 = (("test", 42), 900000000L)
    assert(m7.apply(toFrom7) == toFrom7)
    assert(m7.inverse(toFrom7) == toFrom7)

    illTyped(""" rest[Tuple2, ((String, Int), Long), (Int, String)] """) // RL branch doesn't exist
    illTyped(""" rest[Tuple2, ((String, Int), Long), (String, Int)] : Rest[(String, Int)] """) // Wrong rest

    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), Long] """) // Cannot remove leaf


    rest[Tuple2, ((String, Int), (Double, Long)), Double]: Rest[((String, Int), Long)]
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), Double] : Rest[(Long, (String, Int))] """)
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), Double] : Rest[(Long, (Int, String))] """)
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), Double] : Rest[((Int, String), Long)] """)


    rest[Tuple2, ((String, Int), (Double, Long)), (String, Int)]: Rest[(Double, Long)]

    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (Int, String)] """)
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (String, Int)] : Rest[(Long, Double)] """) // Cannot swap rest
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (Int, String)] """)


    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (Int, String)] """) // This RL branch doesn't exist
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (Double, Long)] """) // Cannot remove leaf like


    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (Int, Double)] """) // Cannot remove cross terms.
    illTyped(""" rest[Tuple2, ((String, Int), (Double, Long)), (String, Long)] """) // Cannot remove cross terms.

    rest[Tuple2, ((String, Int), ((Double, Float), Long)), (String, Int)]: Rest[((Double, Float), Long)]
    rest[Tuple2, ((String, Int), ((Double, Float), Long)), (Double, Float)]: Rest[((String, Int), Long)]

    illTyped(""" rest[Tuple2, ((String, Int), ((Double, Float), Long)), Long] """) // Cannot remove leaf
    illTyped(""" rest[Tuple2, ((String, Int), ((Double, Float), Long)), (Float, Double)] """) // RL branch doesn't exist
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
    type Y1_2 = (T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK)))))))))))))))))
    type Y1_3 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, S))))))))))))))))))

    rest[Tuple2, Y1_1, Y1_2]: Rest[Y1_3]
  }

}
