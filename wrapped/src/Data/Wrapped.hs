-- Copyright 2019-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | Provides 'Wrapped' and 'Wrapped1' types to hold @DerivingVia@ instances.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Wrapped
         ( -- * Derived Instances
           Wrapped(..), Wrapped1(..)
           -- ** Wrapped 'Generic'
           -- $Wrapped_Generic
         , GSemigroup(..), GMonoid(..)
           -- ** Wrapped 'IsList'
           -- $Wrapped_IsList
           -- ** Wrapped 'Foldable'
           -- $Wrapped_Foldable
         ) where

import Control.Applicative (liftA2)
import qualified Data.Foldable as F (toList)
import Data.Function (on)
import Data.Kind (Constraint, Type)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup (Semigroup(..))
#endif
import GHC.Exts (IsList(Item))
import qualified GHC.Exts as Exts (IsList(..))
import GHC.Generics
         ( Generic(..), Generic1(..)
         , M1(..), (:*:)(..), U1(..), K1(..)
         )
import Text.Read (Read(..), readListPrecDefault)

-- | A type holding derived instances for classes of kind @Type -> Constraint@.
--
-- For example, 'Show' or @Pretty@.
--
-- Generally, instances derived from @SomeClass@ should be placed on
-- @'Wrapped' SomeClass@.  This way, they can be grouped into relatively few
-- deriving clauses per type.
newtype Wrapped (c :: Type -> Constraint) a = Wrapped { unWrapped :: a }

-- | A type holding derived instances of kind @(k -> Type) -> Constraint@.
--
-- For example, 'Functor' or 'Traversable'.
--
-- See also 'Wrapped'.
newtype Wrapped1 (c :: (k -> Type) -> Constraint) f (a :: k) =
  Wrapped1 { unWrapped1 :: f a }

-- $Wrapped_Generic
--
-- Instances of @'Wrapped' 'Generic'@ work on 'Rep' types by 'to' and 'from'.
--
-- Typically these implement the "obvious" way to make a sum-of-products type
-- (an algebraic data type) an instance of the given class.  For example, for
-- 'Monoid', it provides field-wise 'mappend' and 'mempty' of types that are
-- products of other 'Monoid's.
--
-- Likewise, @'Wrapped1' 'GHC.Generics.Generic1'@ works on 'GHC.Generics.Rep1'
-- types by 'GHC.Generics.to1' and 'GHC.Generics.from1'.  This is the same
-- concept applied to type constructors with one parameter.

-- | Generic Semigroup.
--
-- Exported just to give Haddock something to link to; use @Wrapped Generic@
-- with @-XDerivingVia@ instead.
class GSemigroup f where
  gsop :: f x -> f x -> f x

instance GSemigroup U1 where
  gsop = const $ const U1

instance GSemigroup a => GSemigroup (M1 i c a) where
  M1 a `gsop` M1 b = M1 $ a `gsop` b

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  (fa :*: ga) `gsop` (fb :*: gb) = (fa `gsop` fb) :*: (ga `gsop` gb)

instance Semigroup a => GSemigroup (K1 i a) where
  K1 fa `gsop` K1 ga = K1 $ fa <> ga

-- | `<>` by field-wise `<>`.
instance (Generic a, GSemigroup (Rep a)) => Semigroup (Wrapped Generic a) where
  Wrapped a <> Wrapped b = Wrapped . to $ from a `gsop` from b

-- | Generic Monoid.
--
-- Exported just to give Haddock something to link to; use @Wrapped Generic@
-- with @-XDerivingVia@ instead.
class GMonoid f where
  gmempty :: f x

instance GMonoid f => GMonoid (M1 i m f) where
  gmempty = M1 gmempty

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty = gmempty :*: gmempty

instance GMonoid U1 where
  gmempty = U1

instance Monoid a => GMonoid (K1 i a) where
  gmempty = K1 mempty

-- | `mappend` by field-wise `<>`, and `mempty` by field-wise `mempty`
--
-- Beware: this determines the entire instance including `mappend`, so do not
-- mix this with a `Semigroup` instance from another source.
instance (Generic a, GSemigroup (Rep a), GMonoid (Rep a))
      => Monoid (Wrapped Generic a) where
#if !MIN_VERSION_base(4, 11, 0)
  mappend = (<>)
#endif
  mempty = Wrapped $ to gmempty

-- $Wrapped_IsList
--
-- Instances of @'Wrapped' 'IsList'@ work by conversion to/from list.
--
-- For example, we provide 'Eq', 'Ord', and 'Show' instances that convert both
-- operands to lists and compare them, and a 'Read' instance that parses a list
-- and converts to the desired type.
--
-- Whereas @Wrapped 'Foldable'@ requires that the type is a type constructor
-- whose argument is the list element, this works on any type with an 'IsList'
-- instance.
--
-- On the other hand, 'IsList' requires that the type can be converted /from/ a
-- list, not only /to/ a list, so it can often require unneeded constraints
-- compared to 'Foldable'.
--
-- Generally, if both of these compile, they should be expected to be
-- equivalent.  More specifically, if you implement instances for @Wrapped
-- Foldable@ or @Wrapped IsList@ these types, you should ensure that, as long
-- as the 'Foldable' instance of @f@ and the 'IsList' instance of @f a@ are
-- consistent, the instances are the same; and if you adopt instances from this
-- type, you should ensure that your 'Foldable' and 'IsList' instances agree,
-- and may then assume that 'IsList' and 'Foldable' give the same instances.

-- | Just forwarding the instance; not meant to be used for deriving.
instance IsList a => IsList (Wrapped IsList a) where
  type Item (Wrapped IsList a) = Exts.Item a
  fromList = Wrapped . Exts.fromList
  fromListN n = Wrapped . Exts.fromListN n
  toList = Exts.toList . unWrapped

-- | Equality of the results of 'Exts.toList'.
instance (IsList a, Eq (Item a)) => Eq (Wrapped IsList a) where
  (==) = (==) `on` Exts.toList

-- | Comparison of the results of 'Exts.toList'.
instance (IsList a, Ord (Item a)) => Ord (Wrapped IsList a) where
  compare = compare `on` Exts.toList

-- | Show of the results of 'Exts.toList'.
instance (IsList a, Show (Item a)) => Show (Wrapped IsList a) where
  showsPrec p = showsPrec p . Exts.toList

-- | 'Exts.fromList' of a parsed list.
instance (IsList a, Read (Item a)) => Read (Wrapped IsList a) where
  readPrec = fmap Exts.fromList $ readPrec @[Item a]
  readListPrec = readListPrecDefault

-- $Wrapped_Foldable
--
-- Instances of @'Wrapped' 'Foldable'@ work by folding over the type.
--
-- See above for a description of how this differs from @'Wrapped' 'IsList'@.

-- | Just forwarding the instance; not meant to be used for deriving.
deriving instance Foldable f => Foldable (Wrapped1 Foldable f)

-- | Equality of the results of 'F.toList'.
instance (Foldable f, Eq a) => Eq (Wrapped1 Foldable f a) where
  (==) = (==) `on` F.toList

-- | Comparison of the results of 'F.toList'.
instance (Foldable f, Ord a) => Ord (Wrapped1 Foldable f a) where
  compare = compare `on` F.toList

-- | Show of the results of 'Exts.toList'.
instance (Foldable f, Show a) => Show (Wrapped1 Foldable f a) where
  showsPrec p = showsPrec p . F.toList

-- | Provide ('<>') by 'liftA2' of an underlying ('<>').
instance (Applicative f, Semigroup a)
      => Semigroup (Wrapped1 Applicative f a) where
  (<>) = fmap Wrapped1 . (liftA2 (<>) `on` unWrapped1)

-- | Provide 'mappend' by 'liftA2' and 'mempty' by @'pure' 'mempty'@.
instance ( Applicative f
         , Monoid a
#if !MIN_VERSION_base(4, 11, 0)
         , Semigroup a
#endif
         )
      => Monoid (Wrapped1 Applicative f a) where
#if !MIN_VERSION_base(4, 11, 0)
  mappend = (<>)
#endif
  mempty = Wrapped1 (pure mempty)

-- | Forwarding instance for 'Functor'.
--
-- If we want @Wrapped1 Generic1 f@ to have instances for things with 'Functor'
-- as a superclass, then it needs to have a 'Functor' instance.  There's not
-- much point in providing a Generics-based one, though because @DeriveFunctor@
-- exists.  So, just forward the underlying type's instance.
deriving instance Functor f => Functor (Wrapped1 Generic1 f)
