{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where

import GHC.Records
import Optics.Core
import GHC.TypeLits

data Whole a = Whole {
    whole1 :: Int, 
    part :: Part a
}

data Part a = Part {
    part1 :: Bool,
    subpart :: Subpart a  
}

data Subpart a = Subpart {
    subpart1 :: String,
    foo :: a
}

whole :: Whole Int
whole = Whole 0 (Part True (Subpart "wee" 7))

-- whole' :: Whole Bool
-- whole' = whole .~  & False 

main :: IO ()
main = putStrLn "Test suite not yet implemented."
