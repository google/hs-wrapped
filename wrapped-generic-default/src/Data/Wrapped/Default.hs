-- Copyright 2017-2021 Google LLC
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

-- | Provides instance @Default (Wrapped Generic a)@ for use with @DerivingVia@.

{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Wrapped.Default (GDefault(..)) where

import GHC.Generics
         ( Generic(..), M1(..), (:+:)(..), (:*:)(..), U1(..), K1(..)
         )

import Data.Default.Class (Default(..))
import Data.Wrapped (Wrapped(..))

-- | Generic instances for Default.
--
-- Exported just to give Haddock something to link to; use @Wrapped Generic@
-- with @-XDerivingVia@ instead.
class GDefault f where
  gdef :: f x

instance GDefault f => GDefault (M1 i m f) where
  gdef = M1 gdef

instance GDefault f => GDefault (f :+: g) where
  gdef = L1 gdef

instance (GDefault f, GDefault g) => GDefault (f :*: g) where
  gdef = gdef :*: gdef

instance GDefault U1 where
  gdef = U1

instance Default a => GDefault (K1 i a) where
  gdef = K1 def

-- | The first constructor with all fields set to 'def'.
--
-- Given the data type definition:
--
-- @
--     data Foo = Foo Int Bool | Bar Double
--       deriving Generic
--       deriving Default via Wrapped Generic Foo
-- @
--
-- then
--
-- @def = Foo def def :: Foo@, i.e. 'def' picks the first constructor and fills
-- it with 'def' calls.
instance (Generic a, GDefault (Rep a)) => Default (Wrapped Generic a) where
  def = Wrapped $ to gdef
