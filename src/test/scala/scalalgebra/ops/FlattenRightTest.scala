package scalalgebra.ops

import scalalgebra.AlgebraOps.flattenRight
import scalalgebra.ops.FlattenRightOps.FlattenRight
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.testutils.ForceImport.forceAssociativeImplicitImport
import shapeless.test.illTyped

class FlattenRightTest extends AnyFunSuite {

  trait Out[X]
  def out[X](implicit e: FlattenRight[X]): Out[e.Out] = new Out[e.Out] {}

  test("Trivial no axioms") {
    import scalalgebra.AlgebraOps.AlgebraOps

    val fr0: FlattenRight.Aux[String, String] = flattenRight[String]
    assert(fr0.apply("test") == "test")
    assert(fr0.inverse("test") == "test")

    illTyped(""" out[String] : Out[Int] """)

    out[(String, Long)]: Out[(String, Long)]
    assert(("test", 9000000L).flattenRight == ("test", 9000000L))
    illTyped(""" out[(String, Long)] : Out[(Long, String)] """)

    out[(String, (Double, Long))]: Out[(String, (Double, Long))]
    assert(("test", (0.123, 9000000L)).flattenRight == ("test", (0.123, 9000000L)))
    illTyped(""" out[((String, Double), Long)] : Out[((String, Double), Long)] """) // True but needs axioms

    out[((String, Float), (Double, Long))]: Out[((String, Float), (Double, Long))]
    illTyped(""" out[((String, Float), (Double, Long))] : Out[((Float, String), (Double, Long))] """)

    illTyped(""" (("test", 0.123), 9000000L).flattenRight """)
  }

  test("Trivial with axioms") {
    import scalalgebra.axioms.Tuple2Axioms._
    forceAssociativeImplicitImport

    val fr0: FlattenRight.Aux[String, String] = flattenRight[String]
    assert(fr0.apply("test") == "test")
    assert(fr0.inverse("test") == "test")

    illTyped(""" out[String] : Out[Int] """)
  }


  test("Tuple2") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraOps.AlgebraOps

    val fr1: FlattenRight.Aux[(String, Long), (String, Long)] = flattenRight[(String, Long)]
    val toFrom1 = ("test", 900000000L)
    assert(fr1(toFrom1) == toFrom1)
    assert(fr1.inverse(toFrom1) == toFrom1)
    assert(toFrom1.flattenRight == toFrom1)

    illTyped(""" out[(Long, String)] : Out[(String, Long)] """)
    illTyped(""" toFrom1.flattenRight : (Long, String) """)

    val fr2: FlattenRight.Aux[(String, (Double, Long)), (String, (Double, Long))] =
      flattenRight[(String, (Double, Long))]
    val toFrom2 = ("test", (0.123, 900000000L))
    assert(fr2(toFrom2) == toFrom2)
    assert(fr2.inverse(toFrom2) == toFrom2)
    assert(toFrom2.flattenRight == toFrom2)

    illTyped(""" out[(String, (Double, Long))] : Out[(Double, (String, Long))] """)
    illTyped(""" out[(String, (Double, Long))] : Out[(String, (Long, Double))] """)


    val fr3: FlattenRight.Aux[((String, Double), Long), (String, (Double, Long))] =
      flattenRight[((String, Double), Long)]
    val to3 = (("test", 0.123), 900000000L)
    val from3 = ("test", (0.123, 900000000L))
    assert(fr3(to3) == from3)
    assert(fr3.inverse(from3) == to3)
    assert(to3.flattenRight == from3)

    illTyped(""" out[((String, Double), Long)] : Out[((String, Double), Long)] """)
    illTyped(""" out[((String, Double), Long)] : Out[((String, Long), Double)] """)
    illTyped(""" to3.flattenRight : ((String, Double), Long) """)


    val fr4: FlattenRight.Aux[((String, Double), (Int, Long)), (String, (Double, (Int, Long)))] =
      flattenRight[((String, Double), (Int, Long))]
    val to4 = (("test", 0.123), (42, 900000000L))
    val from4 = ("test", (0.123, (42, 900000000L)))
    assert(fr4.apply(to4) == from4)
    assert(fr4.inverse(from4) == to4)
    assert(to4.flattenRight == from4)

    illTyped(""" out[((String, Double), (Int, Long))] : Out[((String, Double), (Int, Long))] """)
    illTyped(""" out[((String, Double), (Int, Long))] : Out[((Int, Long), (String, Double))] """)
    illTyped(""" to4.flattenRight : ((String, Double), (Int, Long)) """)

    val fr5: FlattenRight.Aux[(((String, Float), Double), ((Int, Boolean), Long)), (String, (Float, (Double, (Int, (Boolean, Long)))))] =
      flattenRight[(((String, Float), Double), ((Int, Boolean), Long))]
    val to5 = ((("test", 1.1f), 0.123), ((42, true), 900000000L))
    val from5 = ("test", (1.1f, (0.123, (42, (true, 900000000L)))))
    assert(fr5(to5) == from5)
    assert(fr5.inverse(from5) == to5)
  }

  test("Compilation should take at most a few seconds") {
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

    type X1 = ((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M)
    type Y1 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, M))))))))))))

    out[(((A, B), C), D)]
    out[(((String, Int), Long), Double)]

    out[((((A, B), C), D), E)]
    out[((((String, Int), Long), Double), Float)]

    out[X1]: Out[Y1]
    out[Y1]: Out[Y1]

    type X2 = (((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R)
    type Y2 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, (Q, R)))))))))))))))))

    out[X2]: Out[Y2]
    out[Y2]: Out[Y2]
  }

}
