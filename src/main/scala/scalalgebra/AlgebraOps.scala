package scalalgebra

import scalalgebra.structs.CommutativeMagmaIsoOps.CommutativeMagmaIso
import scalalgebra.structs.CommutativeMonoidIsoOps.CommutativeMonoidIso
import scalalgebra.structs.CommutativeSemigroupIsoOps.CommutativeSemigroupIso
import scalalgebra.structs.CommutativeUnitativeMagmaIsoOps.CommutativeUnitativeMagmaIso
import scalalgebra.structs.GenericIsoOps.GenericIso
import scalalgebra.structs.MagmaIsoOps.MagmaIso
import scalalgebra.structs.MonoidIsoOps.MonoidIso
import scalalgebra.structs.SemigroupIsoOps.SemigroupIso
import scalalgebra.structs.UnitativeMagmaIsoOps.UnitativeMagmaIso
import scalalgebra.ops.AlignRLBranchesOps.AlignRLBranches
import scalalgebra.ops.CommuteTreeOps.CommuteTree
import scalalgebra.ops.FlattenRightOps.FlattenRight
import scalalgebra.ops.MoveRBranchToLRLeafOps.MoveRBranchToLRLeaf
import scalalgebra.ops.MoveRLBranchTopOps.MoveRLBranchTop
import scalalgebra.ops.MoveRSubtreeTopOps.MoveRSubtreeTop
import scalalgebra.ops.ReduceUnitOps.ReduceUnit


object AlgebraOps {

  implicit class AlgebraOps[F[_, _], XL, XR](x: F[XL, XR]) {
    def flattenRight(implicit e: FlattenRight[F[XL, XR]]): e.Out = e(x)
    def commuteTree[Y](implicit e: CommuteTree[F[XL, XR], Y]): Y = e.to(x)
    def reduceUnit(implicit e: ReduceUnit[F[XL, XR]]): e.Out = e(x)
    def genericIso[Y](implicit e: GenericIso[F[XL, XR], Y]): Y = e.to(x)
  }
  
  def commuteTree[X, Y](implicit e: CommuteTree[X, Y]): CommuteTree[X, Y] = e
  def flattenRight[X](implicit e: FlattenRight[X]): FlattenRight.Aux[X, e.Out] = e
  def reduceUnit[X](implicit e: ReduceUnit[X]): ReduceUnit.Aux[X, e.Out] = e
  def genericIso[X, Y](implicit e: GenericIso[X, Y]): GenericIso[X, Y] = e

}

/** Lower level function to manipulate the expression tree */
object AlgebraTreeOps {
  implicit class AlgebraTreeOps[F[_, _], XL, XR](x: F[XL, XR]) {
    def moveRBranchToLRLeaf(implicit e: MoveRBranchToLRLeaf[F[XL, XR]]): e.Out = e(x)
    def alignRLBranch[Y](implicit e: AlignRLBranches[F[XL, XR], Y]): Y = e.to(x)
    def moveRLBranchTop[RLB](implicit e: MoveRLBranchTop[F, F[XL, XR], RLB]): e.Out = e(x)
    def moveRSubtreeTop[SubT](implicit e: MoveRSubtreeTop[F, F[XL, XR], SubT]): e.Out = e(x)
  }

  def moveRBranchToLRLeaf[X](implicit e: MoveRBranchToLRLeaf[X]): MoveRBranchToLRLeaf.Aux[X, e.Out] = e
  def alignRLBranch[X, Y](implicit e: AlignRLBranches[X, Y]): AlignRLBranches[X, Y] = e
  def moveRLBranchTop[F[_, _], X, RLB](implicit e: MoveRLBranchTop[F, X, RLB]): MoveRLBranchTop.Aux[F, X, RLB, e.XRest] = e
  def moveRSubtreeTop[F[_, _], X, SubT](implicit e: MoveRSubtreeTop[F, X, SubT]): MoveRSubtreeTop.Aux[F, X, SubT, e.RestX] = e
}


object AlgebraStructureOps {

  implicit class AlgebraStructureOps[F[_, _], XL, XR](x: F[XL, XR]) {
    def magmaIso[Y](implicit e: MagmaIso[F[XL, XR], Y]): Y = e.to(x)
    def monoidIso[Y](implicit e: MonoidIso[F[XL, XR], Y]): Y = e.to(x)
    def semigroupIso[Y](implicit e: SemigroupIso[F[XL, XR], Y]): Y = e.to(x)
    def unitativeMagmaIso[Y](implicit e: UnitativeMagmaIso[F[XL, XR], Y]): Y = e.to(x)
    def commutativeMagmaIso[Y](implicit e: CommutativeMagmaIso[F[XL, XR], Y]): Y = e.to(x)
    def commutativeMonoidIso[Y](implicit e: CommutativeMonoidIso[F[XL, XR], Y]): Y = e.to(x)
    def commutativeSemigroupIso[Y](implicit e: CommutativeSemigroupIso[F[XL, XR], Y]): Y = e.to(x)
    def commutativeUnitativeMagmaIso[Y](implicit e: CommutativeUnitativeMagmaIso[F[XL, XR], Y]): Y = e.to(x)

  }


  def magmaIso[X, Y](implicit e: MagmaIso[X, Y]) = e
  def monoidIso[X, Y](implicit e: MonoidIso[X, Y]) = e
  def semigroupIso[X, Y](implicit e: SemigroupIso[X, Y]) = e
  def unitativeMagmaIso[X, Y](implicit e: UnitativeMagmaIso[X, Y]) = e
  def commutativeMagmaIso[X, Y](implicit e: CommutativeMagmaIso[X, Y]) = e
  def commutativeMonoidIso[X, Y](implicit e: CommutativeMonoidIso[X, Y]) = e
  def commutativeSemigroupIso[X, Y](implicit e: CommutativeSemigroupIso[X, Y]) = e
  def commutativeUnitativeMagmaIso[X, Y](implicit e: CommutativeUnitativeMagmaIso[X, Y]) = e

}