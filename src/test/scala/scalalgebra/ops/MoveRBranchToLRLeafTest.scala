package scalalgebra.ops

import scalalgebra.AlgebraTreeOps.moveRBranchToLRLeaf
import scalalgebra.ops.MoveRBranchToLRLeafOps.MoveRBranchToLRLeaf
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.testutils.ForceImport.forceAssociativeImplicitImport
import shapeless.test.illTyped

class MoveRBranchToLRLeafTest extends AnyFunSuite {

  trait Out[X]
  def out[X](implicit e: MoveRBranchToLRLeaf[X]): Out[e.Out] = new Out[e.Out] {}

  test("Trivial no axioms") {
    import scalalgebra.AlgebraTreeOps._

    val ar1: MoveRBranchToLRLeaf.Aux[String, String] = moveRBranchToLRLeaf[String]
    assert(ar1("test") == "test")
    assert(ar1.inverse("test") == "test")
  }

  test("Trivial with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    forceAssociativeImplicitImport

    val ar1: MoveRBranchToLRLeaf.Aux[String, String] = moveRBranchToLRLeaf[String]
    assert(ar1("test") == "test")
    assert(ar1.inverse("test") == "test")
  }

  test("Base term left, with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    forceAssociativeImplicitImport
    import scalalgebra.AlgebraTreeOps._

    val ar1: MoveRBranchToLRLeaf.Aux[(String, Int), (String, Int)] = moveRBranchToLRLeaf[(String, Int)]
    assert(ar1("test", 42) == ("test", 42))
    assert(ar1.inverse("test", 42) == ("test", 42))
    assert(("test", 42).moveRBranchToLRLeaf == ("test", 42))

    illTyped(""" out[(String, Int)] : Out[(Int, String)] """)

    val ar2: MoveRBranchToLRLeaf.Aux[(String, (Int, Long)), (String, (Int, Long))] = moveRBranchToLRLeaf[(String, (Int, Long))]
    assert(ar2(("test", (42, 900000000L))) == ("test", (42, 900000000L)))
    assert(ar2.inverse(("test", (42, 900000000L))) == ("test", (42, 900000000L)))
    assert(("test", (42, 900000000L)).moveRBranchToLRLeaf == ("test", (42, 900000000L)))

    illTyped(""" out[(String, (Int, Long))] : Out[((String, Int), Long)] """)
    illTyped(""" out[(String, (Int, Long))] : Out[((String, Long), Int)] """)

    val ar3: MoveRBranchToLRLeaf.Aux[(String, ((Int, Double), Long)), (String, ((Int, Double), Long))] =
      moveRBranchToLRLeaf[(String, ((Int, Double), Long))]
    val toFrom3 = ("test", ((42, 0.123), 900000000L))
    assert(ar3(toFrom3) == toFrom3)
    assert(ar3.inverse(toFrom3) == toFrom3)
    assert(toFrom3.moveRBranchToLRLeaf == toFrom3)

    illTyped(""" out[(String, ((Int, Double), Long))] : Out[(((Int, Double), Long), String)] """)
    illTyped(""" out[(String, ((Int, Double), Long))] : Out[((String, Int), (Double, Long))] """)

    val ar4 = moveRBranchToLRLeaf[(String, (Int, (Double, Long)))]
    val toFrom4 = ("test", (42, (0.123, 900000000L)))
    assert(ar4(toFrom4) == toFrom4)
    assert(ar4.inverse(toFrom4) == toFrom4)
    assert(toFrom4.moveRBranchToLRLeaf == toFrom4)

    illTyped(""" out[((String, Int), (Double, Long))] : Out[((String, Int), (Double, Long))] """)
    illTyped(""" out[((String, Int), (Double, Long))] : Out[((Double, Long), (String, Int))] """)
  }

  test("Base term left, no axioms") {
    import scalalgebra.AlgebraTreeOps._

    out[(String, Int)]: Out[(String, Int)]
    assert(("test", 42).moveRBranchToLRLeaf == ("test", 42))
    illTyped(""" out[(String, Int)] : Out[(Int, String)] """)

    out[(String, (Int, Long))]: Out[(String, (Int, Long))]
    assert(("test", (42, 90000000L)).moveRBranchToLRLeaf == ("test", (42, 90000000L)))
    illTyped(""" out[(String, (Int, Long))] : Out[((String, Int), Long)] """)

    out[(String, ((Int, Double), Long))]: Out[(String, ((Int, Double), Long))]
    val toFrom1 = ("test", ((42, 0.123), 900000L))
    assert(toFrom1.moveRBranchToLRLeaf == toFrom1)

    out[(String, (Int, (Double, Long)))]: Out[(String, (Int, (Double, Long)))]
    val toFrom2 = ("test", (42, (0.123, 900000L)))
    assert(toFrom2.moveRBranchToLRLeaf == toFrom2)

  }

  test("Base term right, with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val ar1 = moveRBranchToLRLeaf[((String, Int), Long)]
    val to1 = (("test", 42), 900000000L)
    val from1 = ("test", (42, 900000000L))
    assert(ar1.apply(to1) == from1)
    assert(ar1.inverse(from1) == to1)
    assert(to1.moveRBranchToLRLeaf == from1)

    illTyped(""" out[((String, Int), Long)] : Out[((String, Int), Long)] """)
  }

  test("Base term right, no axioms") {
    import scalalgebra.AlgebraTreeOps._
    // Ensures that the import statement is used and doesn't get optimized out.
    ("test", 12).moveRBranchToLRLeaf

    illTyped(""" out[((String, Int), Long)] """) // Cannot work without axioms
    illTyped(""" (("test", 42), 9000000L).moveRBranchToLRLeaf """)
  }

  test("F left, F right, with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val ar1: MoveRBranchToLRLeaf.Aux[((String, Int), (Long, Double)), (String, (Int, (Long, Double)))] =
      moveRBranchToLRLeaf[((String, Int), (Long, Double))]

    val to1 = (("test", 42), (900000000L, 0.123))
    val from1 = ("test", (42, (900000000L, 0.123)))
    assert(ar1(to1) == from1)
    assert(ar1.inverse(from1) == to1)
    assert(to1.moveRBranchToLRLeaf == from1)

    illTyped(""" out[((String, Int), (Long, Double))] : Out[(((String, Int), Long), Double)] """)
    illTyped(""" out[((String, Int), (Long, Double))] : Out[((String, Int), (Long, Double))] """)
    illTyped(""" out[((String, Int), (Long, Double))] : Out[((String, (Int, Long)), Double)] """)
  }

  test("F left, F right, no axioms") {
    import scalalgebra.AlgebraTreeOps._
    // Ensures that the import statement is used and doesn't get optimized out.
    ("test", 12).moveRBranchToLRLeaf

    illTyped(""" out[((String, Int), (Long, Double))] """) // Cannot work without axioms
    illTyped(""" (("test", 42), (90000000L, 0.123)).moveRBranchToLRLeaf """)
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

    type X1_1 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, S))))))))))))))))))
    type X1_2 = (T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK)))))))))))))))))
    type X1_3 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, (R, (S, (T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK))))))))))))))))))))))))))))))))))))

    out[(X1_1, X1_2)]: Out[X1_3]
    illTyped(""" out[(X1_1, X1_1)] : Out[(X1_1, X1_1)] """)

    type X2_1 = ((((((((((((((((((S, R), Q), P), O), N), M), L), K), J), I), H), G), F), E), D), C), B), A)
    type X2_2 = (T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK)))))))))))))))))
    type X2_3 = ((((((((((((((((((S, R), Q), P), O), N), M), L), K), J), I), H), G), F), E), D), C), B), (A, (T, (U, (V, (W, (X, (Y, (Z, (AA, (AB, (AC, (AD, (AE, (AF, (AG, (AH, (AI, (AJ, AK)))))))))))))))))))

    out[(X2_1, X2_2)]: Out[X2_3]

  }

}
