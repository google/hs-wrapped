# wrapped

A single standardized place to hang `DerivingVia` instances.

[![Stack CI](https://github.com/google/hs-wrapped/actions/workflows/stack-ci.yml/badge.svg)](https://github.com/google/hs-wrapped/actions/workflows/stack-ci.yml)

## Disclaimer

This is not an officially supported Google product.

## Hackage Status

* [![wrapped](https://badgen.net/runkit/awpr/hackage/v/wrapped?icon=haskell&cache=600)](https://hackage.haskell.org/package/wrapped)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/wrapped?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/wrapped?cache=600)
* [![wrapped-generic-default](https://badgen.net/runkit/awpr/hackage/v/wrapped-generic-default?icon=haskell)](https://hackage.haskell.org/package/wrapped-generic-default&cache=600)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/wrapped-generic-default?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/wrapped-generic-default?cache=600)

## Overview

GHC.Generics provides a great mechanism for deriving instances of many classes
for many types, but the ergonomics of opting into the derived instances could be
better.  Currently, functionality derived from other classes is often exported
only as free-standing functions implementing the methods of a class, or as
default methods in the class definition itself.

The former means you have to define an instance by hand to plug the derived
implementations into the methods:

```
data MyType a = ...

instance Distributive MyType where
  collect = collectRep
  distribute = distributeRep
```

The latter means you must either define an empty instance or use
`DeriveAnyClass`, neither of which is particularly clear about how the class
will be implemented:

```
data MyType a = ...
  deriving Generic
  deriving anyclass Representable

-- or:
-- instance Representable MyType
```

This package attempts to improve this situation, taking advantage of
`DerivingVia`.  It provides a type on which to hang instances of arbitrary
classes derived from arbitrary classes, so that derived instances can all be
accessed uniformly with `DerivingVia`.  The desired outcome would be to support
usage like this:

```
data MyType a = ...
  deriving stock (Generic, Functor, Foldable, Traversable, Show)
  deriving (Portray, Semigroup, Monoid) via Wrapped Generic (MyType a)
  deriving Representable via Wrapped1 Generic1 MyType
  deriving Distributive via Wrapped1 Representable MyType
```

In reality, prohibition of orphan instances gets in the way: the `adjunctions`
and `distributive` packages don't provide the instances for `Wrapped`
themselves, and it's not feasible to have `wrapped` depend on every package that
provides derived instances.  So, the aim is to make `wrapped` as lightweight as
possible (it depends only on `base`), so that library maintainers can depend on
it to provide instances for the classes they define, or instances derived from
the classes they define.

## Caveats

### Type Roles

Some classes (e.g. `Distributive` and `Traversable`) cannot work with
`DerivingVia` as currently defined, because they mention the instance type under
a type constructor that's not known to have representational role, so their
methods can't be `coerce`d to the desired type.

See
https://ryanglscott.github.io/2018/06/22/quantifiedconstraints-and-the-trouble-with-traversable/
for a discussion of this issue and a technique for working around it.

### Superclasses

`DerivingVia` has some potentially-surprising or frustrating behavior related to
superclasses.

Firstly, when deriving an instance for a class with a superclass (e.g.
`Monoid`), the superclass instance is not derived along with it, so the type is
free to provide its own instance.  But, if the methods of the subclass subsume
the functionality of the superclass (like `mappend`), those methods will still
come from the `DerivingVia`, resulting in (for example) a `Monoid` whose
`mappend` is different from `<>` on the same type.  One might reasonably assume
that deriving `Monoid` for a type with a custom `Semigroup` instance would be
equivalent to writing `instance Monoid MyType where mempty = genericMempty`, but
there's an invisible default definition of `mappend` in the `DerivingVia` case
that uses the `<>` of the _via type_, not the original type.

Secondly (relatedly), providing an instance on `Wrapped` this way requires
satisfying its superclasses: to provide a `Representable` instance, I have to
arrange for `Wrapped` to have a `Functor` instance along with it; if there's
such an instance derived from `Generic`, then I have to include its context in
the context of the `Representable` instance, too.  Even worse, this can mean
it's impossible to provide instances for classes you've just defined because
their superclasses are classes you _didn't_ define, and they'd be orphan
instances.
