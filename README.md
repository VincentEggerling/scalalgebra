# Scalalgebra

## Summary

This is a proof of concept library allowing you to do group-like algebra at type level. It enables you to manipulate a
tree of nested bifunctor, representing an arbitary algebraic binary operator. For example, such expression could look
like `val x: F[F[A,B], F[C,D]] = ???`.

Based on the axioms you provide in the implicit scope, you can then apply various transformation to the expression
through the different functions present in `scalalgebra.AlgebraOps`.

While this library is highly inspired by [Shapeless](https://github.com/milessabin/shapeless), it doesn't use any of it,
in particular we forbid ourselves to use any macros (beside in tests).

## `F[_,_]` and axioms

As said in the summary, the functions in this library work mainly on type of the form `F[_,_]`, arbitrarly nested.

The minimal axiom you need to provide to get useful result is a proof that `F` is a Bifunctor by providing an implicit
implementation of `Bifunctor[F]`

```
 implicit val bifunctorAxiom = new Bifunctor[F] {
   override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = ???
 }

```

Then you can provide any combination of the following algebraic axioms:

### Associative

```
  implicit val associativeAxioms = new Associative[F] {
    def shiftRight[A, B, C](fabc: F[F[A, B], C]): F[A, F[B, C]] = ???
    def shiftLeft[A, B, C](fabc: F[A, F[B, C]]): F[F[A, B], C] = ???
  }
```

### Commutative

```
  implicit val commutativeAxiom = new Commutative[F] {
    def swap[A, B](fab: F[A, B]): F[B, A] = ???
  }
```

### Unitative

```
  implicit val unitativeAxioms = new Unitative[F] {
    type Unity
    def leftUnitIso[A]: Iso[F[A, Unity], A] = ???
    def rightUnitIso[A]: Iso[F[Unity, A], A] = ???
  }
```

You can also use the name of the algebraic structure you are trying to define that regroups the necessariy axioms. For
example you have the trait `Monoid`: `trait Monoid[F[_, _]] extends Semigroup[F] with UnitativeMagma[F]` which
contains `Semigroup` and `UnitativeMagma` that then contain `Associative` and `Unitative`. See `Axioms.scala` for the
full list.

We provide default axioms for `Tuple2` and `Either` in `scalalgebra.axioms`.

## Main available methods

Once you have your axioms for your `F[_,_]` in scope, you can start to use the library. We basically have 4 main
methods/functions. (We'll use `Tuple2` for our bifunctor in our example, which implements the `CommutativeMonoid`
axioms)

### flattenRight()

This method associates the expression to the right.

```
    // Value-level
    val xValue: ((Int, String), (Long, Double)) = ((12, "test"), (9000000L, 0.123))
    val flatXValue: (Int, (String, (Long, Double))) = xValue.flattenRight
    
    // Type-level
    val flatType: FlattenRight.Aux[((Int, String), (Long, Double)), (Int, (String, (Long, Double)))] = 
      flattenRight[((Int, String), (Long, Double))]
```

### reduceUnit()

This method removes all Unity present in the expression.

```
    // Value-level
    val xValue: ((Int, Unit), (Unit, Double)) = ((12, ()), ((), 0.123))
    val reducedUnitXValue: (Int, Double) = xValue.reduceUnit

    // Type-level
    val reduceUnitType: ReduceUnit.Aux[((Int, Unitt), (Unitt, Double)), (Int, Double)] = 
      reduceUnit[((Int, Unit), (Unit, Double))]
```

### commuteTree()

This method commute the expression to the desired output. If the desired output type breaks commutativity, the line will
not compile.

```
    // Value-level
    val xValue: ((Int, String), (Long, Double)) = ((12, "test"), (9000000L, 0.123))
    val commutedXValue: ((Long, Double), (String, Int)) = xValue.commuteTree[((Long, Double),(String, Int))]

    // Type-level
    val commutedType: CommuteTree[((Int, String), (Long, Double)), ((Long, Double), (String, Int))] = 
      commuteTree[((Int, String), (Long, Double)), ((Long, Double), (String, Int))]
```

### genericIso()

This is the most powerful method that combines all three others. It allows you to transform the expression to an
arbitrary representation given the axioms provided in scope. If the desired output type is unreachable, the line will
not compile.

```
    // Value-level
    val xValue: ((Int, String), (Long, Unit)) = ((12, "test"), (9000000L, ()))
    val yValue: ((Int, Unit), (Long, (Unit, String))) = xValue.genericIso[((Int, Unit), (Long, (Unit, String)))]

    // Type-level
    val yType: GenericIso[((Int, String), (Long, Unit)), ((Int, Unit), (Long, (Unit, String)))] = 
      genericIso[((Int, String), (Long, Unit)), ((Int, Unit), (Long, (Unit, String)))]
```

Note that all type annotation are optional and only shown for the demonstration. They are all infered by the compiler
and by your favourite IDE (tested with IntelliJ 2021.2.1 + Scala plugin).

We try to always provide a "value-level" method of working through the implicit class `AlgebraOps` as well as a static
function to work at type-level when you want to prove mathematical relation and you don't care about a particular value.
The value you get back from those static function is actually an invertible function that you may then apply.

We also would like to stress that all those methods/functions will fail **at compile-time** if the transformation is
impossible. This actually allows you to use your IDE and the compiler prove equivalence between expressions.

See `Example1.scala` for more examples and how to prove equality of two matrix multiplication expressions.

## More miscellaneous methods

- You can check the arity of a higher-kinded type using the trait in `utils.KindPredicates`.
- We have a trait called `MapK2` that allows you to convert a tree of `F[_,_]` into the same tree `G[_,_]`. Currently no
  method exposes the computation. TODO !

## Known problems

- `Nothing` is notoriously difficult to work with in Scala 2. It infers badly in implicits. See `utils.Bottom` for more
  details if you need it.
- Add better docs and support for the trait `MapK2`.
- Some functions can take an exponential time to compile because the implicit search chokes for "no reason". Currently
  `ReduceUnit` and `MapK2` are affected by this behaviour, though `MapK2` should be easily fixable. In general, you
  should have no problem working with trees containing up to 15 terms.

## Future development

- Currently we work in group-like structure, we would like to explore ring-like structure.
- Provide better support for `MapK2`.
- Try to come up with a sensible `Invertible` axiom to handle groups.

## For developers who would want to play with the code

- Careful with the unit-tests, some import statement might be marked as unused by the IDE and depending on your
  configuration, may be optimized out when they should not. This is because we used `illTyped()` and write some
  expressions in strings. If the import is removed, the tests will still pass since the line will fail but it will fail
  for the wrong reason, i.e. a missing import.
- Try not to use any macro except in unit-tests :)