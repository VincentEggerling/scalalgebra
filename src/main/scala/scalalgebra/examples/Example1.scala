package scalalgebra.examples

import cats.Bifunctor
import scalalgebra.axioms.Tuple2Axioms._
import scalalgebra.axioms.EitherAxioms._
import scalalgebra.AlgebraOps._
import scalalgebra.axioms.AlgebraStructureAxioms.Monoid
import scalalgebra.utils.Bottom.Bottom
import scalalgebra.utils.Iso


object Example1 {

  def main(args: Array[String]): Unit = {

    println("Example for Tuple2")

    val x1: ((String, Int), ((Double, Unit), Boolean)) = (("test", 12), ((0.123, ()), true))
    // The type is inferred by the compiler and IntelliJ
    val x1_flat: (String, (Int, (Double, (Unit, Boolean)))) = x1.flattenRight
    val x1_noUnit: ((String, Int), (Double, Boolean)) = x1.reduceUnit
    // You need to give which type you want as output. Note that commuteTree() only allows commutativity
    val x1_commute: (((Unit, Double), Boolean), (String, Int)) =
      x1.commuteTree[(((Unit, Double), Boolean), (String, Int))]
    // This doesn't respect commutativity. It also requires associativity, which commuteTree() doesn't accept.
    // val x1_commuteFail = x1.commuteTree[((String, Int), ((Double, Boolean), Unit))]

    // In general, if you want to shuffle around any element of the Tuple2 tree, use genericIso()
    // genericIso() will try various combinations of the axioms available in scope to output the desired type.
    val x1_shuffled = x1.genericIso[((Double, (String, Unit)), (Boolean, (Int, Unit)))]
    println(x1_shuffled)

    println("Example for Either")
    // Use Bottom instead of Nothing. See scalalgebra.utils.Bottom.
    val y1: Either[Either[String, Int], Either[Either[Double, Bottom], Boolean]] = Left(Right(12))
    val y1_flat: Either[String, Either[Int, Either[Double, Either[Bottom, Boolean]]]] = y1.flattenRight
    val y1_noBottom: Either[Either[String, Int], Either[Double, Boolean]] = y1.reduceUnit
    val y1_commute: Either[Either[Either[Bottom, Double], Boolean], Either[String, Int]] =
      y1.commuteTree[Either[Either[Either[Bottom, Double], Boolean], Either[String, Int]]]

    val y1_shuffled = y1.genericIso[Either[Either[Double, Either[String, Bottom]], Either[Either[Int, Bottom], Boolean]]]
    println(y1_shuffled)


    // We can use the static method genericIso() to prove equality of equations. Let's try that for matrix multiplication
    // that forms a monoid.
    println("Example for matrix multiplication proof")
    // We define some matrices
    trait M1
    trait M2
    trait M3
    trait M4
    trait I // The identity matrix

    // We define the multiplication operator. We use colon operator because the multiplication is associative.
    case class :*:[A, B](a: A, b: B)

    // We write the axioms + Bifunctor which is needed for any operation.
    implicit val matrixMultiplicationAxioms = new Monoid[:*:] with Bifunctor[:*:] {
      override def bimap[A, B, C, D](fab: :*:[A, B])(f: A => C, g: B => D): :*:[C, D] = :*:(f(fab.a), g(fab.b))
      override type Unity = I
      override def leftUnitIso[A]: Iso[:*:[A, Unity], A] = new Iso[:*:[A, Unity], A] {
        override def to(in: :*:[A, Unity]): A = in.a
        override def from(out: A): :*:[A, Unity] = :*:(out, new I {})
      }
      override def rightUnitIso[A]: Iso[:*:[Unity, A], A] = new Iso[:*:[Unity, A], A] {
        override def to(in: :*:[Unity, A]): A = in.b
        override def from(out: A): :*:[Unity, A] = :*:(new I {}, out)
      }
      override def shiftRight[A, B, C](fabc: :*:[:*:[A, B], C]): :*:[A, :*:[B, C]] = :*:(fabc.a.a, :*:(fabc.a.b, fabc.b))
      override def shiftLeft[A, B, C](fabc: :*:[A, :*:[B, C]]): :*:[:*:[A, B], C] = :*:(:*:(fabc.a, fabc.b.a), fabc.b.b)
    }

    genericIso[M1 :*: M2 :*: I :*: M3, M1 :*: M2 :*: M3] // Proof that you can remove the identity
    genericIso[(M1 :*: M2) :*: M3, M1 :*: (M2 :*: M3)] // Proof that it is associative
    //genericIso[M1 :*: M2, M2 :*: M1]// Multiplication is not commutative
    //genericIso[M1 :*: M2 :*: I :*: M3, M3 :*: M2 :*: M1] // Multiplication is not commutative
    genericIso[((M1 :*: I) :*: M2) :*: (M3 :*: M4), M1 :*: M2 :*: I :*: M3 :*: (I :*: M4)] // More involved example

  }

}
