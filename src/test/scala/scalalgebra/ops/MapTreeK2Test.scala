package scalalgebra.ops

import cats.Bifunctor
import org.scalatest.funsuite.AnyFunSuite
import scalalgebra.ops.MapTreeK2Ops.MapTreeK2
import scalalgebra.utils.IsoK2
import shapeless.test.illTyped

class MapTreeK2Test extends AnyFunSuite {

  case class MyClass[A, B](a: A, b: B)

  implicit val bifunctorC = new Bifunctor[MyClass] {
    override def bimap[A, B, C, D](fab: MyClass[A, B])(f: A => C, g: B => D): MyClass[C, D] =
      MyClass(f(fab.a), g(fab.b))
  }

  implicit val isoK2Tuple2ToMyClass = new IsoK2[Tuple2, MyClass] {
    override def to[L, R](in: (L, R)): MyClass[L, R] = MyClass(in._1, in._2)
    override def from[L, R](out: MyClass[L, R]): (L, R) = (out.a, out.b)
  }

  test("MyClassToTuple2") {
    val iso1 = implicitly[MapTreeK2[(String, Int), MyClass[String, Int]]]
    val to1 = ("test", 42)
    val from1 = MyClass("test", 42)
    assert(iso1.to(to1) == from1)
    assert(iso1.from(from1) == to1)

    val iso2 = implicitly[MapTreeK2[(String, (Int, Double)), MyClass[String, MyClass[Int, Double]]]]
    val to2 = ("test", (42, 0.123))
    val from2 = MyClass("test", MyClass(42, 0.123))
    assert(iso2.to(to2) == from2)
    assert(iso2.from(from2) == to2)

    val iso3 = implicitly[MapTreeK2[((String, Int), Double), MyClass[MyClass[String, Int], Double]]]
    val to3 = (("test", 42), 0.123)
    val from3 = MyClass(MyClass("test", 42), 0.123)
    assert(iso3.to(to3) == from3)
    assert(iso3.from(from3) == to3)

    val iso4 = implicitly[MapTreeK2[((String, Int), (Double, Long)), MyClass[MyClass[String, Int], MyClass[Double, Long]]]]
    val to4 = (("test", 42), (0.123, 900000L))
    val from4 = MyClass(MyClass("test", 42), MyClass(0.123, 900000L))
    assert(iso4.to(to4) == from4)
    assert(iso4.from(from4) == to4)

    illTyped(""" implicitly[MapTreeK2[(String, Int), MyClass[Int, String]]] """)
    illTyped(""" implicitly[MapTreeK2[(String, (Int, Double)), MyClass[MyClass[String, Int], Double]]] """)
  }

  test("compile time") {
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

    type X1 = (A, (B, (C, (D, (E, (F, (G, (H, (I, J)))))))))
    type Y1 = MyClass[A, MyClass[B, MyClass[C, MyClass[D, MyClass[E, MyClass[F, MyClass[G, MyClass[H, MyClass[I, J]]]]]]]]]

    implicitly[MapTreeK2[X1, Y1]]

    type X2 = (A, (B, (C, (D, (E, (F, (G, (H, (I, (J, (K, (L, (M, (N, (O, (P, Q))))))))))))))))
    type Y2 = MyClass[A, MyClass[B, MyClass[C, MyClass[D, MyClass[E, MyClass[F, MyClass[G, MyClass[H, MyClass[I, MyClass[J,
      MyClass[K, MyClass[L, MyClass[M, MyClass[N, MyClass[O, MyClass[P, Q]]]]]]]]]]]]]]]]

    implicitly[MapTreeK2[X2, Y2]]

    illTyped(""" implicitly[MapTreeK2[X1, Y2]] """)

    type X3 = (((((A, B), (((C, D), E), (((F, G), H), I))), J), (K, (L, M))), (N, O))
    type Y3 = MyClass[MyClass[MyClass[MyClass[MyClass[A, B], MyClass[MyClass[MyClass[C, D], E], MyClass[MyClass[MyClass[F, G], H], I]]], J], MyClass[K, MyClass[L, M]]], MyClass[N, O]]

    implicitly[MapTreeK2[X3, Y3]]

    // TODO Takes super long when associated left.
    //type X4 = ((((((((((((((O, N), M), L), K), J), I), H), G), F), E), D), C), B), A)
    //type Y4 = MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[MyClass[O, N], M], L], K], J], I], H], G], F], E], D], C], B], A]
    //implicitly[MapTreeK2[X4, Y4]]
  }

}
