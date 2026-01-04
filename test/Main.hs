{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- https://stackoverflow.com/questions/53009549/haskell-derivingvia-on-multi-param-type-classes-with-fun-deps
module Main (main) where

import GHC.Generics
import GHC.Records
import Optics.Core
import Optics.Dot

data Whole a = Whole
  { whole1 :: Int,
    part :: Part a
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics Yo) instance (GField name (Whole t) (Whole s) a b) => RecordDotOptics name (Whole t) (Whole s) a b Yo

-- instance (GField name (Whole p) (Whole q) a b) => RecordDotOptics name (Whole p) (Whole q) a b where
--   dotOptic = gfield @name

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics Yo) instance (GField name (Part t) (Part s) a b) => RecordDotOptics name (Part t) (Part s) a b Yo

data Subpart a = Subpart
  { wee :: String,
    foo :: a,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)

deriving via (GenericDotOptics Yo) instance (GField name (Subpart t) (Subpart s) a b) => RecordDotOptics name (Subpart t) (Subpart s) a b Yo

data YetAnotherSubpart = YetAnotherSubpart
  { ooo :: String,
    uuu :: Int
  }
  deriving (Show)

-- | 'YetAnotherSubpart' doesn't use the 'GField' machinery for
-- 'RecordDotOptics'. Instead, it uses 'HasField'/'SetField'. Field-changing
-- updates are not supported here.
-- instance
--   ( HasField name YetAnotherSubpart x,
--     SetField name YetAnotherSubpart x
--   ) =>
--   RecordDotOptics name YetAnotherSubpart YetAnotherSubpart x x
--   where
--   dotOptic = Optics.Core.lens (getField @name) (flip (setField @name))
instance SetField "ooo" YetAnotherSubpart String where
  setField ooo r = r {ooo}

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))

-- | Note the the type-changing update
whole' :: Whole Bool
whole' = whole & the.part.subpart.foo .~ False

-- -- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
-- whole'' :: Whole Int
-- whole'' = whole & the.part.subpart.yet.ooo .~ "newval"

main :: IO ()
main = do
  print whole
  print whole'

-- print whole''
