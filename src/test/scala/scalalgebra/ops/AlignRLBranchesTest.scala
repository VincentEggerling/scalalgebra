package scalalgebra.ops

import scalalgebra.ops.AlignRLBranchesOps.AlignRLBranches
import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped

class AlignRLBranchesTest extends AnyFunSuite {
  test("base no axioms") {
    import scalalgebra.AlgebraTreeOps._

    alignRLBranch[String, String]
    alignRLBranch[(String, Int), (String, Int)]
    alignRLBranch[((String, Double), Int), ((String, Double), Int)]
    alignRLBranch[(String, (Double, Int)), (String, (Double, Int))]

    illTyped(""" alignRLBranch[(String, (Double, Int)), ((Double, Int), String)] """) // True, but needs axioms
    illTyped(""" alignRLBranch[(String, Int), (Int, String)] """) // True, but needs axioms
    illTyped(""" alignRLBranch[String, Int] """)


    assert(("test", 42).alignRLBranch[(String, Int)] == ("test", 42))
    illTyped(""" ("test", 42).alignRLBranch[(Int, String)] """) // True, but need axioms

  }

  test("Tuple2 flattened") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val a1 = alignRLBranch[String, String]
    assert(a1.to("test") == "test")
    assert(a1.from("test") == "test")

    illTyped(""" alignRLBranch[String, Int] """)

    val a2 = alignRLBranch[(String, Int), (String, Int)]
    val toFrom2 = ("test", 42)
    assert(a2.to(toFrom2) == toFrom2)
    assert(a2.from(toFrom2) == toFrom2)
    assert(toFrom2.alignRLBranch[(String, Int)] == toFrom2)

    illTyped(""" alignRLBranch[(String, Int), String] """)
    illTyped(""" alignRLBranch[(String, Int), Int] """)

    val a3 = alignRLBranch[(String, Int), (Int, String)]
    val to3 = ("test", 42)
    val from3 = (42, "test")
    assert(a3.to(to3) == from3)
    assert(a3.from(from3) == to3)
    assert(to3.alignRLBranch[(Int, String)] == from3)
    assert(from3.alignRLBranch[(String, Int)] == to3)

    illTyped(""" alignRLBranch[(String, Int), (Int, Long)] """)
    illTyped(""" alignRLBranch[(String, Int), (Long, Int)] """)
    illTyped(""" to3.alignRLBranch[(Int, Long)] """)

    val a5 = alignRLBranch[(String, (Double, Int)), (String, (Double, Int))]
    val toFrom5 = ("test", (0.123, 42))
    assert(a5.to(toFrom5) == toFrom5)
    assert(a5.from(toFrom5) == toFrom5)

    val a6 = alignRLBranch[(String, (Double, Int)), (String, (Int, Double))]
    val to6 = ("test", (0.123, 42))
    val from6 = ("test", (42, 0.123))
    assert(a6.to(to6) == from6)
    assert(a6.from(from6) == to6)
    assert(to6.alignRLBranch[(String, (Int, Double))] == from6)

    alignRLBranch[(String, (Double, Int)), (Int, (Double, String))]
    illTyped(""" alignRLBranch[(String, (Double, Int)), (Int, (Double, Long))] """)

    alignRLBranch[(String, (Double, (Int, Long))), (String, (Double, (Int, Long)))]
    alignRLBranch[(String, (Double, (Int, Long))), (String, (Double, (Long, Int)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Double, (String, (Int, Long)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Double, (String, (Long, Int)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Double, (Int, (String, Long)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Long, (Int, (String, Double)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Long, (Int, (Double, String)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Long, (String, (Double, Int)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Double, (Int, (Long, String)))]
    alignRLBranch[(String, (Double, (Int, Long))), (Int, (Long, (Double, String)))]

    illTyped(""" alignRLBranch[(String, (Double, (Int, Long))), (Double, (Int, (Long, Float)))] """)

    val to7 = ("test", (0.123, (42, 90000000L)))
    assert(to7.alignRLBranch[(String, (Double, (Int, Long)))] == to7)
    assert(to7.alignRLBranch[(String, (Double, (Long, Int)))] == ("test", (0.123, (90000000L, 42))))
    assert(to7.alignRLBranch[(Long, (String, (Double, Int)))] == (90000000L, ("test", (0.123, 42))))

    illTyped(""" to7.alignRLBranch[(Double, (Int, (Long, Float)))] """)
  }

  test("Tuple2 non flattened") {
    import scalalgebra.axioms.Tuple2Axioms._
    import scalalgebra.AlgebraTreeOps._

    val a4 = alignRLBranch[(String, (Double, Int)), ((Double, Int), String)]
    val to4 = ("test", (0.123, 42))
    val from4 = ((0.123, 42), "test")
    assert(a4.to(to4) == from4)
    assert(a4.from(from4) == to4)
    assert(to4.alignRLBranch[((Double, Int), String)] == from4)
    assert(from4.alignRLBranch[(String, (Double, Int))] == to4)

    illTyped(""" alignRLBranch[(String, (Double, Int)), ((String, Double), Int)] """)
    illTyped(""" alignRLBranch[(String, (Double, Int)), ((Double, String), Int)] """)
    illTyped(""" alignRLBranch[(String, (Double, Int)), ((Int, String), Double)] """)
    illTyped(""" alignRLBranch[(String, (Double, Int)), (Int, Double)] """)
    illTyped(""" alignRLBranch[(String, (Double, Int)), (Double, Int)] """)


    alignRLBranch[((String, Long), (Double, Int)), ((String, Long), (Double, Int))]
    alignRLBranch[((String, Long), (Double, Int)), ((String, Long), (Int, Double))]
    alignRLBranch[((String, Long), (Double, Int)), (Int, ((String, Long), Double))]
    alignRLBranch[((String, Long), (Double, Int)), (Double, ((String, Long), Int))]
    alignRLBranch[((String, Long), (Double, Int)), (Int, (Double, (String, Long)))]
    alignRLBranch[((String, Long), (Double, Int)), (Double, (Int, (String, Long)))]

    illTyped(""" alignRLBranch[((String, Long), (Double, Int)), (Int, ((Long, String), Double))] """) // Cannot swap the RL branch (String, Long)
    illTyped(""" alignRLBranch[((String, Long), (Double, Int)), ((Long, String), (Double, Int))] """) // Cannot swap the RL branch (String, Long)
    alignRLBranch[((String, Long), (Double, Int)), (Int, (Double, (Long, String)))]

    alignRLBranch[((String, Long), (Double, Int)), ((Double, Int), (String, Long))]
    alignRLBranch[((String, Long), (Double, Int)), ((Double, Int), (Long, String))]
    illTyped(""" alignRLBranch[((String, Long), (Double, Int)), ((Int, Double), (Long, String))] """)

  }

}
