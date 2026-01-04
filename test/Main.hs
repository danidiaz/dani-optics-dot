{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
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

instance HasOpticsMethod (Whole s) where
  type Method (Whole s) = (Whole s)

instance
  (GField name (Whole s) (Whole t) a b) =>
  RecordDotOptics (Whole s) name (Whole s) (Whole t) a b
  where
  dotOptic = gfield @name

data Part a = Part
  { part1 :: Bool,
    subpart :: Subpart a
  }
  deriving stock (Generic, Show)

instance
  (GField name (Part s) (Part t) a b) =>
  RecordDotOptics (Part s) name (Part s) (Part t) a b
  where
  dotOptic = gfield @name

instance HasOpticsMethod (Part s) where
  type Method (Part s) = (Part s)

data Subpart a = Subpart
  { wee :: String,
    foo :: a,
    yet :: YetAnotherSubpart
  }
  deriving stock (Generic, Show)

instance HasOpticsMethod (Subpart s) where
  type Method (Subpart s) = (Subpart s)

instance
  (GField name (Subpart s) (Subpart t) a b) =>
  RecordDotOptics (Subpart s) name (Subpart s) (Subpart t) a b
  where
  dotOptic = gfield @name

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

instance HasOpticsMethod YetAnotherSubpart where
  type Method YetAnotherSubpart = FieldDotOptics

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7 (YetAnotherSubpart "oldval" 3)))

whole'1 :: Whole Bool
whole'1 = whole & the.part .~ (Part True (Subpart "wee" False (YetAnotherSubpart "oldval" 3)))

-- | Note the the type-changing update
whole' :: Whole Bool
whole' = whole & the.part.subpart .~ (Subpart "wee" False (YetAnotherSubpart "oldval" 3))

-- | Non-type changed update which includes 'GField' lenses and 'HasField'/'SetField' lenses.
whole'' :: Whole Int
whole'' = whole & the.part.subpart.yet.ooo .~ "newval"

normalDotAccess :: String
normalDotAccess = whole.part.subpart.yet.ooo

main :: IO ()
main = do
  print whole
  print whole'

-- print whole''
-- print normalDotAccess
