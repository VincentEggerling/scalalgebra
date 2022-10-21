package scalalgebra.ops

import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraOps.reduceUnit
import scalalgebra.axioms.Tuple2Axioms
import scalalgebra.ops.ReduceUnitOps.ReduceUnit
import scalalgebra.utils.Bottom.Bottom
import shapeless.test.illTyped

class ReduceUnitTest extends AnyFunSuite {

  trait Out[X]
  def out[X](implicit e: ReduceUnit[X]): Out[e.Out] = new Out[e.Out] {}

  test("test no axioms") {
    import scalalgebra.AlgebraOps.AlgebraOps
    // Ensures that the import statement is used and doesn't get optimized out.
    ("test", "test").commuteTree[(String, String)]

    type Unitt = Tuple2Axioms.tuple2Axioms.Unity

    val ru0: ReduceUnit.Aux[Unitt, Unitt] = reduceUnit[Unitt]
    assert(ru0.apply(()) == ())
    assert(ru0.inverse(()) == ())

    val ru1: ReduceUnit.Aux[Unit, Unit] = reduceUnit[Unit]
    assert(ru1.apply(()) == ())
    assert(ru1.inverse(()) == ())

    val ru2: ReduceUnit.Aux[String, String] = reduceUnit[String]
    assert(ru2.apply("test") == "test")
    assert(ru2.inverse("test") == "test")

    illTyped(""" out[String] : Out[Unit] """)
    illTyped(""" out[Unit] : Out[String] """)

    illTyped(""" reduceUnit[(String, Int)] """) // True, but needs axioms to check deeper
    illTyped(""" reduceUnit[(Unitt, Unitt)] """) // True, but needs axioms.
    illTyped(""" reduceUnit[(Unit, Unit)] """) // True, but needs axioms.
    illTyped(""" reduceUnit[(String, Unitt)] """)
    illTyped(""" reduceUnit[(String, Unit)] """)

    // Debatable behavior
    illTyped(""" ("test", ()).reduceUnit """)
    illTyped(""" ("test", 42).reduceUnit """)
    illTyped(""" ("test", ((), 42)).reduceUnit """)

  }


  test("Test Tuple2") {
    import scalalgebra.AlgebraOps.AlgebraOps
    import scalalgebra.axioms.Tuple2Axioms._

    type Unitt = Tuple2Axioms.tuple2Axioms.Unity
    implicitly[Unitt =:= Unit]

    illTyped(""" out[String] : Out[Unit] """)
    illTyped(""" out[Unit] : Out[String] """)


    out[Unitt]: Out[Unitt]
    out[Unitt]: Out[Unit]
    out[Unit]: Out[Unitt]
    val ru0: ReduceUnit.Aux[Unit, Unit] = reduceUnit[Unit]
    assert(ru0.apply(()) == ())
    assert(ru0.inverse(()) == ())

    val ru1 = reduceUnit[String]
    assert(ru1.apply("test") == "test")
    assert(ru1.inverse("test") == "test")

    val ru2 = reduceUnit[(String, Int)]
    assert(ru2.apply("test", 42) == ("test", 42))
    assert(ru2.inverse("test", 42) == ("test", 42))
    assert(("test", 42).reduceUnit == ("test", 42))


    illTyped(""" out[(String, Int)] : Out[(Int, String)] """) // Cannot swap
    illTyped(""" out[(String, Unit)] : Out[(String, Unit)] """) // Unit must be reduced


    out[(String, Unitt)]: Out[String]
    val ru3 = reduceUnit[(String, Unit)]
    val to3 = ("test", ())
    val from3 = "test"
    assert(ru3.apply(to3) == from3)
    assert(ru3.inverse(from3) == to3)
    assert(to3.reduceUnit == from3)


    illTyped(""" out[(String, Unit)] : Out[Unit] """)
    illTyped(""" out[(Unit, String)] : Out[Unit] """)
    illTyped(""" out[(String, Unitt)] : Out[Unit] """)
    illTyped(""" out[(Unit, String)] : Out[Unitt] """)

    val ru4 = reduceUnit[(Unit, Unit)]
    val to4 = ((), ())
    val from4 = ()
    assert(ru4.apply(to4) == from4)
    assert(ru4.inverse(from4) == to4)
    assert(to4.reduceUnit == from4)

    illTyped(""" out[(Unit, Unitt)] : Out[String] """)

    val ru41 = reduceUnit[(Unit, (Unit, Unit))]
    val to41 = ((), ((), ()))
    val from41 = ()
    assert(ru41.apply(to41) == from41)
    assert(ru41.inverse(from41) == to41)
    assert(to41.reduceUnit == from41)

    val ru42 = reduceUnit[((Unit, Unit), Unit)]
    val to42 = (((), ()), ())
    val from42 = ()
    assert(ru42.apply(to42) == from42)
    assert(ru42.inverse(from42) == to42)
    assert(to42.reduceUnit == from42)

    val ru5 = reduceUnit[(Unit, String)]
    val to5 = ((), "test")
    val from5 = "test"
    assert(ru5.apply(to5) == from5)
    assert(ru5.inverse(from5) == to5)

    val ru6 = reduceUnit[((String, Unit), Unit)]
    val to6 = (("test", ()), ())
    val from6 = "test"
    assert(ru6.apply(to6) == from6)
    assert(ru6.inverse(from6) == to6)

    val ru7 = reduceUnit[(Unit, (String, Unit))]
    val to7 = ((), ("test", ()))
    val from7 = "test"
    assert(ru7.apply(to7) == from7)
    assert(ru7.inverse(from7) == to7)

    val ru8 = reduceUnit[((String, Unit), Int)]
    val to8 = (("test", ()), 42)
    val from8 = ("test", 42)
    assert(ru8.apply(to8) == from8)
    assert(ru8.inverse(from8) == to8)

    illTyped(""" out[((String, Unit), Int)] : Out[(Int, String)] """) // Cannot swap

    val ru9 = reduceUnit[((Unitt, String), Long)]
    val to9 = (((), "test"), 900000000L)
    val from9 = ("test", 900000000L)
    assert(ru9.apply(to9) == from9)
    assert(ru9.inverse(from9) == to9)

    illTyped(""" out[((Unitt, String), Long)] : Out[((Unitt, String), Long)] """) // Not reduced form
    illTyped(""" out[((String, Unit), Long)] : Out[((String, Unit), Long)] """) // Not reduced form
    illTyped(""" out[((String, Int), Unit)] : Out[((String, Int), Unit)] """) // Not reduced form


    val ru91 = reduceUnit[(String, (Int, Long))]
    val toFrom91 = ("test", (42, 900000000L))
    assert(ru91.apply(toFrom91) == toFrom91)
    assert(ru91.inverse(toFrom91) == toFrom91)

    illTyped(""" out[(String, (Int, Long))] : Out[((String, Int), Long)] """) // Cannot reassociate
    illTyped(""" out[(String, (Int, Long))] : Out[(String, (Long, Int))] """) // Cannot swap


    val ru92 = reduceUnit[((String, Double), (Int, Long))]
    val toFrom92 = (("test", 0.123), (42, 900000000L))
    assert(ru92.apply(toFrom92) == toFrom92)
    assert(ru92.inverse(toFrom92) == toFrom92)


    illTyped(""" out[((String, Double), (Int, Long))] : Out[(String, (Double, (Int, Long)))] """) // Cannot reassociate
    illTyped(""" out[((String, Double), (Int, Long))] : Out[((String, Int), (Double, Long))] """) // Cannot swap

    val ru10 = reduceUnit[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt))))]
    val to10 = ((42, ()), (("test", ()), (90000000L, (0.123, ()))))
    val from10 = (42, ("test", (90000000L, 0.123)))
    assert(ru10.apply(to10) == from10)
    assert(ru10.inverse(from10) == to10)

    illTyped(""" out[((Int, Unitt), ((String, Unitt), (Long, (Double, Unitt))))] : Out[(Int, (String, (Double, Long)))] """) // Cannot swap

    out[(Unit, (Unit, Unit))]: Out[Unit]
  }

  test("Either") {
    import scalalgebra.axioms.EitherAxioms._

    val ru1 = reduceUnit[Either[Int, String]]
    val toFrom1Left = Left(42)
    val toFrom1Right = Right("test")
    assert(ru1.apply(toFrom1Left) == toFrom1Left)
    assert(ru1.apply(toFrom1Right) == toFrom1Right)
    assert(ru1.inverse(toFrom1Left) == toFrom1Left)
    assert(ru1.inverse(toFrom1Right) == toFrom1Right)

    out[Either[Bottom, Int]]: Out[Int]
    out[Either[Int, Bottom]]: Out[Int]

    out[Either[Bottom, Either[Int, String]]]: Out[Either[Int, String]]
    out[Either[Bottom, Either[Bottom, Int]]]: Out[Int]
    out[Either[Bottom, Either[Int, Bottom]]]: Out[Int]
    out[Either[Bottom, Either[Bottom, Bottom]]]: Out[Bottom]

    out[Either[Either[Int, String], Bottom]]: Out[Either[Int, String]]
    out[Either[Either[Bottom, Int], Bottom]]: Out[Int]
    out[Either[Either[Int, Bottom], Bottom]]: Out[Int]
    out[Either[Either[Bottom, Bottom], Bottom]]: Out[Bottom]

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

    type X1 = (((((((((((((((((((((((((A, B), C), D), E), F), G), H), I), J), K), L), M), N), O), P), Q), R), S), T), U), V), W), X), Y), Z)
    out[X1]: Out[X1]

    type X2 = (((((((((((((((((((((((((((A, B), C), D), E), F), G), H), Unit), I), J), K), L), M), N), O), P), Unit), Q), R), S), T), U), V), W), X), Y), Z)
    out[X2]: Out[X1]

    type Y1 = ((((((Unit, (Unit, (Unit, A))), B), (((C, D), E), (((F, (G, Unit)), H), I))), J), ((Unit, (K, Unit)), (L, M))), (N, O))
    type Y2 = (((((A, B), (((C, D), E), (((F, G), H), I))), J), (K, (L, M))), (N, O))

    // TODO takes too much time...
    //out[Y1]: Out[Y2]

  }

}
