# wrapped

Provides a single centralized place to hang `DerivingVia` instances.

This exports the newtypes `Wrapped` and `Wrapped1`, which are meant to hold
typeclass implementations based on `Generic` and `Generic1`, respectively. If
you implement derivation of a class, add it as an instance for `Wrapped` or
`Wrapped1`, and users will be able to derive it as `deriving TheClass via
Wrapped TheType`.

This package is meant to be ruthlessly lightweight, in the hopes that package
maintainers will be less hesitant to depend on it to provide instances: it has
no dependencies other than `base`.  Having its instances live in orphan instance
packages makes it significantly less convenient, which is a bit of a problem for
a package whose purpose is to make things more convenient and consistent.
Ideally, every package that defines a typeclass with `Generic` deriving support
or a default implementation in terms of another class would make it available as
a `Wrapped` instance.
