package scalalgebra.utils

import org.scalatest.funsuite.AnyFunSuite
import shapeless.test.illTyped
import scalalgebra.utils.KindsPredicatesOps.{IsK0, IsK1, IsK1Plus, IsK2, IsK2Plus, IsK3Plus}

class KindsPredicatesTest extends AnyFunSuite {

  test("IsK1Plus") {
    trait T[A]

    implicit def f[A](implicit e: IsK1Plus[A]) = new T[A] {}

    illTyped(""" implicitly[IsK1Plus[Int]] """)
    implicitly[IsK1Plus[List[Int]]]
    implicitly[IsK1Plus[(Int, String)]]
    implicitly[IsK1Plus[(Int, String, Double)]]

    illTyped(""" implicitly[T[Int]] """)
    implicitly[T[List[Int]]]
    implicitly[T[(Int, String)]]
    implicitly[T[(Int, String, Double)]]
  }

  test("IsK2Plus") {
    trait T[A]

    implicit def f[A](implicit e: IsK2Plus[A]) = new T[A] {}

    illTyped(""" implicitly[IsK2Plus[Int]] """)
    illTyped(""" implicitly[IsK2Plus[List[Int]]] """)
    implicitly[IsK2Plus[(Int, String)]]
    implicitly[IsK2Plus[(Int, String, Double)]]

    illTyped(""" implicitly[T[Int]] """)
    illTyped(""" implicitly[T[List[Int]]] """)
    implicitly[T[(Int, String)]]
    implicitly[T[(Int, String, Double)]]
  }

  test("IsK3Plus") {
    trait T[A]

    implicit def f[A](implicit e: IsK3Plus[A]) = new T[A] {}

    illTyped(""" implicitly[IsK3Plus[Int]] """)
    illTyped(""" implicitly[IsK3Plus[List[Int]]] """)
    illTyped(""" implicitly[IsK3Plus[(String, Int)]] """)
    implicitly[IsK3Plus[(String, Int, Double)]]
    implicitly[IsK3Plus[(String, Int, Double, Long)]]

    illTyped(""" implicitly[T[Int]] """)
    illTyped(""" implicitly[T[List[Int]]] """)
    illTyped(""" implicitly[T[(String, Int)]] """)
    implicitly[T[(String, Int, Double)]]
    implicitly[T[(String, Int, Double, Long)]]

  }

  test("IsK0") {
    trait T[A]

    implicit def f[A](implicit e: IsK0[A]) = new T[A] {}

    implicitly[IsK0[Int]]
    illTyped(""" implicitly[IsK0[List[Int]]] """)
    illTyped(""" implicitly[IsK0[(String, Int)]] """)

    implicitly[T[Int]]
    illTyped(""" implicitly[T[List[Int]]] """)
    illTyped(""" implicitly[T[(String, Int)]] """)

  }

  test("IsK1") {
    trait T[A]

    implicit def f[A](implicit e: IsK1[A]) = new T[A] {}

    illTyped(""" implicitly[IsK1[Int]] """)
    implicitly[IsK1[List[Int]]]
    illTyped(""" implicitly[IsK1[(String, Int)]] """)
    illTyped(""" implicitly[IsK1[(String, Int, Double)]] """)

    illTyped(""" implicitly[T[Int]] """)
    implicitly[T[List[Int]]]
    illTyped(""" implicitly[T[(String, Int)]] """)
    illTyped(""" implicitly[T[(String, Int, Double)]] """)
  }

  test("IsK2") {
    trait T[A]

    implicit def f[A](implicit e: IsK2[A]) = new T[A] {}

    illTyped(""" implicitly[IsK2[Int]] """)
    illTyped(""" implicitly[IsK2[List[Int]]] """)
    implicitly[IsK2[(String, Int)]]
    illTyped(""" implicitly[IsK2[(String, Int, Double)]] """)

    illTyped(""" implicitly[T[Int]] """)
    illTyped(""" implicitly[T[List[Int]]] """)
    implicitly[T[(String, Int)]]
    illTyped(""" implicitly[T[(String, Int, Double)]] """)
  }

}
