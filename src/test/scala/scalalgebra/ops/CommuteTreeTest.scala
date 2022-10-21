package scalalgebra.ops

import scalalgebra.AlgebraOps.commuteTree
import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped

class CommuteTreeTest extends AnyFunSuite {
  test("trivial no axioms") {
    import scalalgebra.AlgebraOps.AlgebraOps

    commuteTree[String, String]
    commuteTree[(String, Int), (String, Int)]
    commuteTree[(String, (Int, Double)), (String, (Int, Double))]

    assert(("test", 42).commuteTree[(String, Int)] == ("test", 42))
    assert(("test", (42, 0.123)).commuteTree[(String, (Int, Double))] == ("test", (42, 0.123)))

    illTyped(""" commuteTree[String, Int] """)
    illTyped(""" commuteTree[(String, (Int, Double)), (String, (Double, Int))] """) // True, but needs the axioms
    illTyped(""" commuteTree[(String, Int), (Int, String)] """) // True, but needs the axioms
    illTyped(""" commuteTree[(String, (Int, Double)), ((Int, Double), String)] """)
    illTyped(""" ("test", 42).commuteTree[(Int, String)] """) // True, but needs the axioms
    illTyped(""" ("test", (42, 0.123)).commuteTree[(String, (Double, Int))] """) // True, but needs the axioms
  }

  test("Tuple2") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraOps.AlgebraOps

    val c1 = commuteTree[String, String]
    assert(c1.to("test") == "test")
    assert(c1.from("test") == "test")

    illTyped(""" commuteTree[String, Int] """)

    val c2 = commuteTree[(String, Int), (String, Int)]
    val toFrom2 = ("test", 42)
    assert(c2.to(toFrom2) == toFrom2)
    assert(c2.from(toFrom2) == toFrom2)
    assert(toFrom2.commuteTree[(String, Int)] == toFrom2)

    illTyped(""" commuteTree[(String, Int), (String, Double)] """)
    illTyped(""" commuteTree[(String, Int), String] """)
    illTyped(""" commuteTree[(String, Int), Int] """)
    illTyped(""" commuteTree[(String, Int), (String, (Int, String))] """)

    val c3 = commuteTree[(String, Int), (Int, String)]
    val to3 = ("test", 42)
    val from3 = (42, "test")
    assert(c3.to(to3) == from3)
    assert(c3.from(from3) == to3)
    assert(to3.commuteTree[(Int, String)] == from3)

    val c4 = commuteTree[(String, (Int, Double)), (String, (Int, Double))]
    val toFrom4 = ("test", (42, 0.123))
    assert(c4.to(toFrom4) == toFrom4)
    assert(c4.from(toFrom4) == toFrom4)
    assert(toFrom4.commuteTree[(String, (Int, Double))] == toFrom4)

    illTyped(""" commuteTree[(String, (Int, Double)), (String, (Int, Long))] """)
    illTyped(""" commuteTree[(String, (Int, Double)), ((String, Int), Double)] """) // Cannot reassociate
    illTyped(""" commuteTree[(String, (Int, Double)), ((String, Double), Int)] """)
    illTyped(""" toFrom4.commuteTree[(String, (Int, Float))] """)
    illTyped(""" toFrom4.commuteTree[((String, Int), Double)] """)

    val c5 = commuteTree[(String, (Int, Double)), (String, (Double, Int))]
    val to5 = ("test", (42, 0.123))
    val from5 = ("test", (0.123, 42))
    assert(c5.to(to5) == from5)
    assert(c5.from(from5) == to5)
    assert(to5.commuteTree[(String, (Double, Int))] == from5)

    val c6 = commuteTree[(String, (Int, Double)), ((Double, Int), String)]
    val to6 = ("test", (42, 0.123))
    val from6 = ((0.123, 42), "test")
    assert(c6.to(to6) == from6)
    assert(c6.from(from6) == to6)

    val c7 = commuteTree[(String, (Int, Double)), ((Int, Double), String)]
    val to7 = ("test", (42, 0.123))
    val from7 = ((42, 0.123), "test")
    assert(c7.to(to7) == from7)
    assert(c7.from(from7) == to7)

    illTyped(""" commuteTree[(String, (Int, Double)), (Int, (Double, String))] """)

    val c8 = commuteTree[((String, Long), (Int, Double)), ((Int, Double), (Long, String))]
    val to8 = (("test", 900000000L), (42, 0.123))
    val from8 = ((42, 0.123), (900000000L, "test"))
    assert(c8.to(to8) == from8)
    assert(c8.from(from8) == to8)

    illTyped(""" commuteTree[((String, Long), (Int, Double)), ((Int, Long), (Double, String))] """)
    illTyped(""" commuteTree[((String, Long), (Int, Double)), (String, (Long, (Int, Double)))] """)

  }

  test("Compile time test") {
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


    type X1 = (((((A, B), (C, D)), E), F), ((G, H), (I, ((J, K), ((L, M), N)))))

    commuteTree[X1, (((G, H), (I, ((J, K), ((L, M), N)))), ((((A, B), (C, D)), E), F))]
    commuteTree[X1, (((G, H), (I, ((J, K), ((L, M), N)))), (F, (((A, B), (C, D)), E)))]
    commuteTree[X1, (((G, H), (I, ((J, K), ((L, M), N)))), (F, (((A, B), (D, C)), E)))]
    commuteTree[X1, (((G, H), (I, ((J, K), (N, (L, M))))), (F, (((A, B), (D, C)), E)))]
    commuteTree[X1, (((I, ((J, K), (N, (L, M)))), (G, H)), (F, (((A, B), (D, C)), E)))]
    commuteTree[X1, (((I, ((K, J), (N, (L, M)))), (G, H)), (F, (((A, B), (D, C)), E)))]

  }

}
