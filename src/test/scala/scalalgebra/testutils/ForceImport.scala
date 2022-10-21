package scalalgebra.testutils

import scalalgebra.axioms.AlgebraAxioms.{Associative, Commutative, Unitative}

/**
 * Dummy function to make sure that the import don't get optimized out when testing with
 * illTyped().
 */
object ForceImport {

  def forceAssociativeImplicitImport[F[_, _]](implicit a: Associative[F]): Unit = ()
  def forceCommutativeImplicitImport[F[_, _]](implicit a: Commutative[F]): Unit = ()
  def forceUnitativeImplicitImport[F[_, _]](implicit a: Unitative[F]): Unit = ()

}
