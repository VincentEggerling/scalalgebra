package scalalgebra.ops

import scalalgebra.axioms.AlgebraAxioms.Unitative
import scalalgebra.ops.IsNotUnitOps.IsNotUnit
import cats.Bifunctor
import scalalgebra.utils.DepFuncInv
import scalalgebra.utils.KindsPredicatesOps.IsK0

object ReduceUnitOps {

  /**
   * Proof that if you remove all unit in X you get Out.
   */
  trait ReduceUnit[X] extends DepFuncInv[X]

  object ReduceUnit {
    type Aux[X, Out0] = ReduceUnit[X] {type Out = Out0}
    // Allows ReduceUnit[String, String], however it forbids ReduceUnit[(String, Int), (String, Int)]. While this is true,
    // we don't know what the unit is.
    implicit def trivial[A](implicit notK2: IsK0[A]): Aux[A, A] =
      new ReduceUnit[A] {
        type Out = A
        override def apply(in: A): Out = in
        override def inverse(out: Out): A = out
      }

    implicit def reduceUnit[F[_, _], XL, XR](implicit e: ReduceUnit_[F, XL, XR]): Aux[F[XL, XR], e.Out] =
      new ReduceUnit[F[XL, XR]] {
        type Out = e.Out
        override def apply(in: F[XL, XR]): Out = e.apply(in)
        override def inverse(out: Out): F[XL, XR] = e.inverse(out)
      }
  }


  // Allows the caller to use directly the type like Unit, instead of having to import the Unity from the axioms.
  type UnitativeAux[F[_, _], U] = Unitative[F] {type Unity = U}

  protected sealed trait ReduceUnit_[F[_, _], XL, XR] extends DepFuncInv[F[XL, XR]]
  // TODO Some Prio could probably be simplified. Careful, IntelliJ sometimes resolves correctly the implicit but the compiler struggles.
  trait Prio00 {
    type Aux[F[_, _], XL, XR, Out0] = ReduceUnit_[F, XL, XR] {type Out = Out0}

    implicit def baseAB[F[_, _], A, B](implicit un: Unitative[F], neq: IsNotUnit[F, A], neq2: IsNotUnit[F, B]): Aux[F, A, B, F[A, B]] =
      new ReduceUnit_[F, A, B] {
        type Out = F[A, B]
        override def apply(in: F[A, B]): Out = in
        override def inverse(out: Out): F[A, B] = out
      }
  }
  trait Prio01 extends Prio00 {
    implicit def baseAU[F[_, _], A, U](implicit unit: UnitativeAux[F, U]): Aux[F, A, U, A] =
      new ReduceUnit_[F, A, U] {
        type Out = A
        override def apply(in: F[A, U]): Out = unit.leftUnitIso.to(in)
        override def inverse(out: Out): F[A, U] = unit.leftUnitIso.from(out)
      }
  }
  trait Prio02 extends Prio01 {
    implicit def baseUA[F[_, _], A, U](implicit unit: UnitativeAux[F, U]): Aux[F, U, A, A] =
      new ReduceUnit_[F, U, A] {
        type Out = A
        override def apply(in: F[U, A]): Out = unit.rightUnitIso.to(in)
        override def inverse(out: Out): F[U, A] = unit.rightUnitIso.from(out)
      }
  }

  // We don't need a baseUU case since if one base type is Unit, the output will be the other, whatever it is, Unit or not.

  trait Prio10 extends Prio02 {
    implicit def recL_AB_C[F[_, _], LL, LR, A]
    (implicit ru: ReduceUnit_[F, LL, LR], bif: Bifunctor[F]): Aux[F, F[LL, LR], A, F[ru.Out, A]] =
      new ReduceUnit_[F, F[LL, LR], A] {
        type Out = F[ru.Out, A]
        override def apply(in: F[F[LL, LR], A]): Out = bif.leftFunctor.map(in)(ru.apply(_))
        override def inverse(out: Out): F[F[LL, LR], A] = bif.leftFunctor.map(out)(ru.inverse)
      }
  }
  trait Prio11 extends Prio10 {
    implicit def recL_AB_U[F[_, _], LL, LR, U]
    (implicit unit: UnitativeAux[F, U], ru: ReduceUnit_[F, LL, LR], bif: Bifunctor[F]): Aux[F, F[LL, LR], U, ru.Out] =
      new ReduceUnit_[F, F[LL, LR], U] {
        type Out = ru.Out
        override def apply(in: F[F[LL, LR], U]): Out = unit.leftUnitIso.to(bif.leftFunctor.map(in)(ru.apply(_)))
        override def inverse(out: Out): F[F[LL, LR], U] = bif.leftFunctor.map(unit.leftUnitIso.from(out))(ru.inverse)
      }
  }
  trait Prio12 extends Prio11 {
    implicit def recL_U_A[F[_, _], LL, LR, A, U]
    (implicit unit: UnitativeAux[F, U], ru: Aux[F, LL, LR, U], bif: Bifunctor[F]): Aux[F, F[LL, LR], A, A] =
      new ReduceUnit_[F, F[LL, LR], A] {
        type Out = A
        override def apply(in: F[F[LL, LR], A]): Out = unit.rightUnitIso.to(bif.leftFunctor.map(in)(ru.apply))
        override def inverse(out: Out): F[F[LL, LR], A] = bif.leftFunctor.map(unit.rightUnitIso.from(out))(ru.inverse)
      }
  }

  // We don't need recL_U_U

  trait Prio20 extends Prio12 {
    implicit def recR_A_BC[F[_, _], A, RL, RR]
    (implicit ru: ReduceUnit_[F, RL, RR], bif: Bifunctor[F]): Aux[F, A, F[RL, RR], F[A, ru.Out]] =
      new ReduceUnit_[F, A, F[RL, RR]] {
        type Out = F[A, ru.Out]
        override def apply(in: F[A, F[RL, RR]]): Out = bif.rightFunctor.map(in)(ru.apply(_))
        override def inverse(out: Out): F[A, F[RL, RR]] = bif.rightFunctor.map(out)(ru.inverse)
      }
  }
  trait Prio21 extends Prio20 {
    implicit def recR_U_AB[F[_, _], U, RL, RR]
    (implicit unit: UnitativeAux[F, U], ru: ReduceUnit_[F, RL, RR], bif: Bifunctor[F]): Aux[F, U, F[RL, RR], ru.Out] =
      new ReduceUnit_[F, U, F[RL, RR]] {
        type Out = ru.Out
        override def apply(in: F[U, F[RL, RR]]): Out = unit.rightUnitIso.to(bif.rightFunctor.map(in)(ru.apply(_)))
        override def inverse(out: Out): F[U, F[RL, RR]] = bif.rightFunctor.map(unit.rightUnitIso.from(out))(ru.inverse)
      }
  }
  trait Prio22 extends Prio21 {
    implicit def recR_A_U[F[_, _], RL, RR, U, A]
    (implicit unit: UnitativeAux[F, U], ru: Aux[F, RL, RR, U], bif: Bifunctor[F]): Aux[F, A, F[RL, RR], A] =
      new ReduceUnit_[F, A, F[RL, RR]] {
        type Out = A
        override def apply(in: F[A, F[RL, RR]]): Out = unit.leftUnitIso.to(bif.rightFunctor.map(in)(ru.apply))
        override def inverse(out: Out): F[A, F[RL, RR]] = bif.rightFunctor.map(unit.leftUnitIso.from(out))(ru.inverse)
      }
  }

  // We don't need recR_U_U

  trait Prio30 extends Prio22 {
    implicit def recLR_AB_CD[F[_, _], LL, LR, RL, RR]
    (implicit ruL: ReduceUnit_[F, LL, LR], ruR: ReduceUnit_[F, RL, RR], bif: Bifunctor[F]): Aux[F, F[LL, LR], F[RL, RR], F[ruL.Out, ruR.Out]] =
      new ReduceUnit_[F, F[LL, LR], F[RL, RR]] {
        type Out = F[ruL.Out, ruR.Out]
        override def apply(in: F[F[LL, LR], F[RL, RR]]): Out = bif.bimap(in)(ruL.apply(_), ruR.apply(_))
        override def inverse(out: Out): F[F[LL, LR], F[RL, RR]] = bif.bimap(out)(ruL.inverse, ruR.inverse)
      }
  }
  trait Prio31 extends Prio30 {
    implicit def recLR_U_AB[F[_, _], LL, LR, RL, RR, U]
    (implicit unit: UnitativeAux[F, U], ruL: Aux[F, LL, LR, U], ruR: ReduceUnit_[F, RL, RR], bif: Bifunctor[F]): Aux[F, F[LL, LR], F[RL, RR], ruR.Out] =
      new ReduceUnit_[F, F[LL, LR], F[RL, RR]] {
        type Out = ruR.Out
        override def apply(in: F[F[LL, LR], F[RL, RR]]): Out = unit.rightUnitIso.to(bif.bimap(in)(ruL.apply, ruR.apply(_)))
        override def inverse(out: Out): F[F[LL, LR], F[RL, RR]] = bif.bimap(unit.rightUnitIso.from(out))(ruL.inverse, ruR.inverse)
      }
  }
  object ReduceUnit_ extends Prio31 {
    implicit def recLR_AB_U[F[_, _], LL, LR, RL, RR, U]
    (implicit unit: UnitativeAux[F, U], ruL: ReduceUnit_[F, LL, LR], ruR: Aux[F, RL, RR, U], bif: Bifunctor[F]): Aux[F, F[LL, LR], F[RL, RR], ruL.Out] =
      new ReduceUnit_[F, F[LL, LR], F[RL, RR]] {
        type Out = ruL.Out
        override def apply(in: F[F[LL, LR], F[RL, RR]]): Out = unit.leftUnitIso.to(bif.bimap(in)(ruL.apply(_), ruR.apply))
        override def inverse(out: Out): F[F[LL, LR], F[RL, RR]] = bif.bimap(unit.leftUnitIso.from(out))(ruL.inverse, ruR.inverse)
      }
  }

}
