package scalalgebra.utils

object KindsPredicatesOps {


  trait IsK1Plus[A]
  implicit def isK1plus[F[_], A] = new IsK1Plus[F[A]] {}

  trait IsK2Plus[A]
  implicit def isK2plus[F[_, _], A, B] = new IsK2Plus[F[A, B]] {}

  trait IsK3Plus[A]
  implicit def isK3plus[F[_, _, _], A, B, C] = new IsK3Plus[F[A, B, C]] {}

  trait IsK0_OrK1[A] // Same as Not IsK2Plus
  implicit def amb1k0k1[A](implicit e: IsK2Plus[A]) = new IsK0_OrK1[A] {}
  implicit def amb2k0k1[A](implicit e: IsK2Plus[A]) = new IsK0_OrK1[A] {}
  implicit def validk0k1[A] = new IsK0_OrK1[A] {}

  trait IsK0_OrK1_OrK2[A] // // Same as Not IsK3Plus
  implicit def amb1k0k1k2[A](implicit e: IsK3Plus[A]) = new IsK0_OrK1_OrK2[A] {}
  implicit def amb2k0k1k2[A](implicit e: IsK3Plus[A]) = new IsK0_OrK1_OrK2[A] {}
  implicit def validk0k1k2[A] = new IsK0_OrK1_OrK2[A] {}


  trait IsK0[A] // Same as "Not IsK1Plus"
  implicit def amb1k0[A](implicit e: IsK1Plus[A]) = new IsK0[A] {}
  implicit def amb2k0[A](implicit e: IsK1Plus[A]) = new IsK0[A] {}
  implicit def validk0[A] = new IsK0[A] {}

  trait IsK1[A] // Same as IsK0_OrK1 and Not k0
  implicit def amb1k1[A](implicit e1: IsK0_OrK1[A], e2: IsK0[A]) = new IsK1[A] {}
  implicit def amb2k1[A](implicit e1: IsK0_OrK1[A], e2: IsK0[A]) = new IsK1[A] {}
  implicit def validk1[A](implicit e1: IsK0_OrK1[A]) = new IsK1[A] {}

  trait IsK2[A] // Same as IsK0_OrK1_OrK2 and Not IsK0_OrK1
  implicit def amb1k2[A](implicit e1: IsK0_OrK1_OrK2[A], e2: IsK0_OrK1[A]) = new IsK2[A] {}
  implicit def amb2k2[A](implicit e1: IsK0_OrK1_OrK2[A], e2: IsK0_OrK1[A]) = new IsK2[A] {}
  implicit def validk2[A](implicit e1: IsK0_OrK1_OrK2[A]) = new IsK2[A] {}

}
