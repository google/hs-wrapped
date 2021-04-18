# wrapped

Provides a single standardized place to hang `DerivingVia` instances.

This exports the newtypes `Wrapped` and `Wrapped1`, which are meant to hold
typeclass implementations based on `Generic` and `Generic1`, respectively. If
you implement derivation of a class, add it as an instance for `Wrapped` or
`Wrapped1`, and users will be able to derive it as `deriving TheClass via
Wrapped TheType`.
