package scalalgebra.structs

import cats.Bifunctor
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.AlgebraOps.genericIso
import scalalgebra.axioms.AlgebraStructureAxioms.{CommutativeMagma, CommutativeMonoid, CommutativeSemigroup, CommutativeUnitativeMagma, Monoid, Semigroup, UnitativeMagma}
import scalalgebra.utils.Iso
import shapeless.test.illTyped

class GenericIsoTest extends AnyFunSuite {

  implicit val bifTuple2 = new Bifunctor[Tuple2] {
    override def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) = (f(fab._1), g(fab._2))
  }

  test("Magma") {
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    illTyped(""" genericIso[String, Int] """)
    illTyped(""" genericIso[(Unit, String), String] """)
    illTyped(""" genericIso[((Long, Int), String),(Long, (Int, String))] """)
    illTyped(""" genericIso[(String, Int), (Int, String)] """)
  }

  test("Semigroup Tuple2") {
    implicit val tuple2Axioms = new Semigroup[Tuple2] {
      override def shiftRight[A, B, C](fabc: ((A, B), C)): (A, (B, C)) = (fabc._1._1, (fabc._1._2, fabc._2))
      override def shiftLeft[A, B, C](fabc: (A, (B, C))): ((A, B), C) = ((fabc._1, fabc._2._1), fabc._2._2)
    }

    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // Semigroup
    genericIso[(Long, (Int, String)), ((Long, Int), String)]
    genericIso[((Long, Int), String), (Long, (Int, String))]

    illTyped(""" genericIso[String, Int] """)
    illTyped(""" genericIso[(Unit, String), String] """)
    illTyped(""" genericIso[(String, Int), (Int, String)] """)
  }

  test("UnitativeMagma Tuple2") {
    implicit val tuple2Axioms = new UnitativeMagma[Tuple2] {
      override type Unity = Unit
      override def leftUnitIso[A]: Iso[(A, Unity), A] = new Iso[(A, Unity), A] {
        override def to(in: (A, Unity)): A = in._1
        override def from(out: A): (A, Unity) = (out, ())
      }
      override def rightUnitIso[A]: Iso[(Unity, A), A] = new Iso[(Unity, A), A] {
        override def to(in: (Unity, A)): A = in._2
        override def from(out: A): (Unity, A) = ((), out)
      }
    }

    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // UnitativeMagma
    genericIso[(Unit, String), String]
    genericIso[((String, Unit), (Double, Unit)), ((String, Unit), (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, Double)]

    illTyped(""" genericIso[String, Int] """)
    illTyped(""" genericIso[((Long, Int), String),(Long, (Int, String))] """)
    illTyped(""" genericIso[(String, Int), (Int, String)] """)
  }

  test("CommutativeMagma Tuple2") {
    implicit val tuple2Axioms = new CommutativeMagma[Tuple2] {
      override def swap[A, B](fab: (A, B)): (B, A) = fab.swap
    }
    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // CommutativeMagma
    genericIso[(String, Int), (Int, String)]
    genericIso[((String, Int), (Long, Double)), ((Double, Long), (String, Int))]

    illTyped(""" genericIso[String, Int] """)
    illTyped(""" genericIso[(Unit, String), String] """)
    illTyped(""" genericIso[((Long, Int), String),(Long, (Int, String))] """)
  }

  test("Monoid Tuple2") {
    implicit val tuple2Axioms = new Monoid[Tuple2] {
      override type Unity = Unit
      override def leftUnitIso[A]: Iso[(A, Unity), A] = new Iso[(A, Unity), A] {
        override def to(in: (A, Unity)): A = in._1
        override def from(out: A): (A, Unity) = (out, ())
      }
      override def rightUnitIso[A]: Iso[(Unity, A), A] = new Iso[(Unity, A), A] {
        override def to(in: (Unity, A)): A = in._2
        override def from(out: A): (Unity, A) = ((), out)
      }
      override def shiftRight[A, B, C](fabc: ((A, B), C)): (A, (B, C)) = (fabc._1._1, (fabc._1._2, fabc._2))
      override def shiftLeft[A, B, C](fabc: (A, (B, C))): ((A, B), C) = ((fabc._1, fabc._2._1), fabc._2._2)
    }

    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // UnitativeMagma
    genericIso[(Unit, String), String]
    genericIso[((String, Unit), (Double, Unit)), ((String, Unit), (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, Double)]

    // Semigroup
    genericIso[(Long, (Int, String)), ((Long, Int), String)]
    genericIso[((Long, Int), String), (Long, (Int, String))]

    // Monoid
    genericIso[((String, Double), (Long, Unit)), ((Unit, String), (Unit, ((Double, Unit), Long)))]

    illTyped(""" genericIso[(String, Int), (Int, String)] """)
  }

  test("CommutativeUnitativeMagma Tuple2") {
    implicit val tuple2Axioms = new CommutativeUnitativeMagma[Tuple2] {
      override type Unity = Unit
      override def leftUnitIso[A]: Iso[(A, Unity), A] = new Iso[(A, Unity), A] {
        override def to(in: (A, Unity)): A = in._1
        override def from(out: A): (A, Unity) = (out, ())
      }
      override def rightUnitIso[A]: Iso[(Unity, A), A] = new Iso[(Unity, A), A] {
        override def to(in: (Unity, A)): A = in._2
        override def from(out: A): (Unity, A) = ((), out)
      }
      override def swap[A, B](fab: (A, B)): (B, A) = fab.swap
    }


    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // UnitativeMagma
    genericIso[(Unit, String), String]
    genericIso[((String, Unit), (Double, Unit)), ((String, Unit), (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, Double)]

    // CommutativeMagma
    genericIso[(String, Int), (Int, String)]
    genericIso[((String, Int), (Long, Double)), ((Double, Long), (String, Int))]

    // CommutativeUnitativeMagma
    genericIso[(((String, Unit), Long), (Double, Int)), (((Int, Unit), Double), ((Unit, String), Long))]

    illTyped(""" genericIso[((Long, Int), String),(Long, (Int, String))] """)

  }

  test("CommutativeSemigroup") {
    implicit val tuple2Axioms = new CommutativeSemigroup[Tuple2] {
      override def shiftRight[A, B, C](fabc: ((A, B), C)): (A, (B, C)) = (fabc._1._1, (fabc._1._2, fabc._2))
      override def shiftLeft[A, B, C](fabc: (A, (B, C))): ((A, B), C) = ((fabc._1, fabc._2._1), fabc._2._2)
      override def swap[A, B](fab: (A, B)): (B, A) = fab.swap
    }

    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // CommutativeMagma
    genericIso[(String, Int), (Int, String)]
    genericIso[((String, Int), (Long, Double)), ((Double, Long), (String, Int))]

    // Semigroup
    genericIso[(Long, (Int, String)), ((Long, Int), String)]
    genericIso[((Long, Int), String), (Long, (Int, String))]

    // CommutativeSemigroup
    genericIso[((Unit, Double), ((String, Long), Int)), (Double, ((Long, Unit), (Int, String)))]

    illTyped(""" genericIso[(Unit, String), String] """)
  }

  test("CommutativeMonoid Tuple2") {
    implicit val tuple2Axioms = new CommutativeMonoid[Tuple2] {
      override def shiftRight[A, B, C](fabc: ((A, B), C)): (A, (B, C)) = (fabc._1._1, (fabc._1._2, fabc._2))
      override def shiftLeft[A, B, C](fabc: (A, (B, C))): ((A, B), C) = ((fabc._1, fabc._2._1), fabc._2._2)
      override def swap[A, B](fab: (A, B)): (B, A) = fab.swap

      override type Unity = Unit
      override def leftUnitIso[A]: Iso[(A, Unity), A] = new Iso[(A, Unity), A] {
        override def to(in: (A, Unity)): A = in._1
        override def from(out: A): (A, Unity) = (out, ())
      }
      override def rightUnitIso[A]: Iso[(Unity, A), A] = new Iso[(Unity, A), A] {
        override def to(in: (Unity, A)): A = in._2
        override def from(out: A): (Unity, A) = ((), out)
      }
    }


    // Magma
    genericIso[String, String]
    genericIso[Unit, Unit]
    genericIso[(Unit, String), (Unit, String)]
    genericIso[(Long, (Int, String)), (Long, (Int, String))]
    genericIso[((Long, Int), String), ((Long, Int), String)]

    // CommutativeMagma
    genericIso[(String, Int), (Int, String)]
    genericIso[((String, Int), (Long, Double)), ((Double, Long), (String, Int))]

    // Semigroup
    genericIso[(Long, (Int, String)), ((Long, Int), String)]
    genericIso[((Long, Int), String), (Long, (Int, String))]

    // UnitativeMagma
    genericIso[(Unit, String), String]
    genericIso[((String, Unit), (Double, Unit)), ((String, Unit), (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, (Double, Unit))]
    genericIso[((String, Unit), (Double, Unit)), (String, Double)]

    // CommutativeUnitativeMagma
    genericIso[(((String, Unit), Long), (Double, Int)), (((Int, Unit), Double), ((Unit, String), Long))]

    // CommutativeSemigroup
    genericIso[((Unit, Double), ((String, Long), Int)), (Double, ((Long, Unit), (Int, String)))]

    // Monoid
    genericIso[((String, Double), (Long, Unit)), ((Unit, String), (Unit, ((Double, Unit), Long)))]

    // CommutativeMonoid
    genericIso[((String, Double), Long), ((Unit, String), ((Unit, Long), ((Double, Unit), Unit)))]

  }

  test("Test several iso applicable") {
    import scalalgebra.AlgebraOps._
    import scalalgebra.axioms.Tuple2Axioms._
    (("test", 12), ((0.123, ()), true)).genericIso[((Int, (String, Unit)), (Double, Boolean))]
    genericIso[((String, Int), ((Double, Unit), Boolean)), ((Int, (String, Unit)), (Double, Boolean))]
  }

  test("Mixing F[_,_]'s") {
    import scalalgebra.axioms.Tuple2Axioms._
    genericIso[(Either[String, Int], Either[Long, Double]), (Either[Long, Double], Either[String, Int])]
    illTyped(""" genericIso[(Either[String, Int], Either[Long, Double]), (Either[Double, Long], Either[String, Int])] """)

    genericIso[((Either[String, Int], Unit), Either[Long, Double]), (Either[Long, Double], Either[String, Int])]

    genericIso[
      ((Either[String, Int], Either[Boolean, Float]), Either[Long, Double]),
      ((Either[Long, Double], Unit), (Either[String, Int], Either[Boolean, Float]))]
  }

  test("MyClass base axioms") {
    import scalalgebra.axioms.AlgebraAxioms._
    import scalalgebra.AlgebraOps._
    case class MyClass[A, B](a: A, b: B)

    implicit val myClassAss = new Associative[MyClass] with Bifunctor[MyClass] {
      override def shiftRight[A, B, C](fabc: MyClass[MyClass[A, B], C]): MyClass[A, MyClass[B, C]] = MyClass(fabc.a.a, MyClass(fabc.a.b, fabc.b))
      override def shiftLeft[A, B, C](fabc: MyClass[A, MyClass[B, C]]): MyClass[MyClass[A, B], C] = MyClass(MyClass(fabc.a, fabc.b.a), fabc.b.b)
      override def bimap[A, B, C, D](fab: MyClass[A, B])(f: A => C, g: B => D): MyClass[C, D] = MyClass(f(fab.a), g(fab.b))
    }

    val myClass1: MyClass[MyClass[Int, String], MyClass[Long, Double]] = MyClass(MyClass(42, "test"), MyClass(90000L, 0.123))
    myClass1.genericIso[MyClass[Int, MyClass[MyClass[String, Long], Double]]]
    genericIso[MyClass[MyClass[Int, String], MyClass[Long, Double]], MyClass[Int, MyClass[MyClass[String, Long], Double]]]
  }

  test("MyClass algebraic structure axiom") {
    import scalalgebra.AlgebraOps._
    case class MyClass[A, B](a: A, b: B)

    implicit val myClassSemigroup = new Semigroup[MyClass] with Bifunctor[MyClass] {
      override def shiftRight[A, B, C](fabc: MyClass[MyClass[A, B], C]): MyClass[A, MyClass[B, C]] = MyClass(fabc.a.a, MyClass(fabc.a.b, fabc.b))
      override def shiftLeft[A, B, C](fabc: MyClass[A, MyClass[B, C]]): MyClass[MyClass[A, B], C] = MyClass(MyClass(fabc.a, fabc.b.a), fabc.b.b)
      override def bimap[A, B, C, D](fab: MyClass[A, B])(f: A => C, g: B => D): MyClass[C, D] = MyClass(f(fab.a), g(fab.b))
    }
    val myClass1: MyClass[MyClass[Int, String], MyClass[Long, Double]] = MyClass(MyClass(42, "test"), MyClass(90000L, 0.123))
    myClass1.genericIso[MyClass[Int, MyClass[MyClass[String, Long], Double]]]
    genericIso[MyClass[MyClass[Int, String], MyClass[Long, Double]], MyClass[Int, MyClass[MyClass[String, Long], Double]]]
  }


}
