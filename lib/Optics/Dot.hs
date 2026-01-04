{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UndecidableInstances #-}

module Optics.Dot where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Optics.Core

instance
  ( RecordDotOptics name u v a b Yo,
    JoinKinds k A_Lens m,
    AppendIndices is NoIx ks
  ) =>
  HasField name (Optic k is s t u v) (Optic m ks s t a b)
  where
  getField o = o % dotOptic @name @u @v @a @b @Yo

type RecordDotOptics :: Symbol -> Type -> Type -> Type -> Type -> Type -> Constraint
class RecordDotOptics name u v a b yo | name u -> v a b where
  dotOptic :: Lens u v a b

data Yo = Yo

newtype GenericDotOptics yo = GenericDotOptics yo

-- deriving via (GenericDotOptics (RecordDotOptics Yo name u v a b)) instance (RecordDotOptics (Yo name u v a b))

instance (GField name u v a b) => RecordDotOptics name u v a b (GenericDotOptics yo) where
  dotOptic = gfield @name

the :: Iso a b a b
the = Optics.Core.equality

-- | This should be in base in the future.
type SetField :: forall {k}. k -> Type -> Type -> Constraint
class SetField x r a | x r -> a where
  -- | Selector function to extract the field from the record.
  setField :: a -> r -> r

newtype StockDotOptics yo = StockDotOptics yo

instance
  ( HasField name u a,
    SetField name u a
  ) =>
  RecordDotOptics name u u a a (StockDotOptics yo)
  where
  dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))
