package scalalgebra.utils

/*
  Nothing infers badly in Scala 2. Look at this snippet:
===============================================
  trait T[A]
  trait U[A]

  implicit def u[A] = new U[A] {}
  implicit def t[A](implicit e: U[A]) = new T[A]{}

  type Bottom <: Nothing

  def main(args: Array[String]): Unit = {
    implicitly[Bottom =:= Nothing]  // They are the same
    implicitly[T[Bottom]] // This works
    // implicitly[T[Nothing]] // This fails
  }
================================================
Some references:
- https://stackoverflow.com/questions/61951621/failed-implicit-resolution-for-nothing-with
- https://stackoverflow.com/questions/62003214/implicit-error-when-trying-to-implement-the-absurd-typeclass
 */
object Bottom {
  type Bottom <: Nothing
}