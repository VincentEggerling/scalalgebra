package scalalgebra.utils


/** Represents an invertible function which output type depends on the input type
 * The output type is computed with implicits.
 * The input type is sufficient to compute the output type via implicits.
 * The converse is not true though, therefore when using inverse() you will need to explicitly
 * provide the input type.
 */
trait DepFuncInv[X] {
  type Out
  def apply(x: X): Out
  def inverse(out: Out): X
}

/** Represents an invertible Function1 that is supposed to respect the isomorphism's axioms. */
trait Iso[In, Out] {
  def to(in: In): Out
  def from(out: Out): In
}

/** I guess you could call that a natural isomorphism between two bifunctors... */
trait IsoK2[F[_, _], G[_, _]] {
  def to[L, R](in: F[L, R]): G[L, R]
  def from[L, R](out: G[L, R]): F[L, R]
}

