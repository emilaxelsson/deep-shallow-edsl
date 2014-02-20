{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- Script for live-coding FunC in a lecture



import Prelude hiding ((==), (<), not, min, zipWith, sum)
import qualified Prelude as P

import Control.Monad.State
import Data.Array
import Data.Tree hiding (drawTree)

import Data.Tree.View



----------------------------------------------------------------------
-- * Deep embedding
--
-- Designed to be easily translated to a low-level language such as C
----------------------------------------------------------------------

data FunC a
  where
--     LitI
--     LitB
--     If
--     While
--     Prim1
--     Prim2
--     Value
--     Variable

-- Highlight use of GADT

-- eval :: FunC a -> a

-- toTree, etc.

-- Boolean literals

-- instance Num (FunC Int)

-- Define (==), (<), not, min



----------------------------------------------------------------------
-- * Shallow embeddings
--
-- Designed with respect to the user
----------------------------------------------------------------------

-- class Syntactic a

-- instance Syntactic (FunC a)

-- desugar1, desugar2, desugar3

-- Define non-overloaded ifC

-- Define ifC, (?)

-- Define while

-- Syntactic instance for pairs

-- Define forLoop



----------------------------------------------------------------------
-- * Optional values
----------------------------------------------------------------------

-- Add Undef

-- Define Option

-- instance Syntactic a => Syntactic (Option a)

-- Define some, none, option

-- option :: Syntactic b => b -> (a -> b) -> Option a -> b

-- instance Functor Option

-- instance Monad Option

-- divO :: FunC Int -> FunC Int -> Option (FunC Int)

-- divTest :: FunC Int -> FunC Int -> FunC Int -> Option (FunC Int)



----------------------------------------------------------------------
-- * Vectors
----------------------------------------------------------------------

-- Add arrays

-- len :: FunC (Array Int a) -> FunC Int

-- (<!>) :: FunC (Array Int a) -> FunC Int -> FunC a

-- data Vector a

-- vlen :: Vector a -> FunC Int

-- (<!!>) :: Vector a -> FunC Int -> a

-- Syntactic instance for vectors

-- instance Functor Vector

-- zipWith

-- sum

-- scalarProd

-- force

-- Demo fusion

