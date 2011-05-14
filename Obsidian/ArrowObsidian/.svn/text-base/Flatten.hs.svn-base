{-# LANGUAGE OverlappingInstances, 
             TypeSynonymInstances, 
             FlexibleContexts, 
             UndecidableInstances #-}

{- 
   Flatten.hs 

   Joel Svensson
-}


module Obsidian.ArrowObsidian.Flatten 
       (Flatten,
        FData,
        List(Nil,Unit,Tuple,For), 
        toFData,
        fromFData,
        size, 
        sizeArr
       ) where

import Obsidian.ArrowObsidian.Arr
import Obsidian.ArrowObsidian.Exp 
import Obsidian.ArrowObsidian.PureAPI

import Obsidian.ArrowObsidian.Printing

import Data.Word

import qualified Data.Foldable as Fold

import System.IO.Unsafe

--------------------------------------------------------------------------------

data List a 
    = Nil 
    | Unit a 
    | Tuple [List a]
    | For (Arr (List a))
      deriving Show

instance (Eq (Arr (List a)),Eq a) => Eq (List a) where 
    (==) (Unit a) (Unit b) = a == b
    (==) (Tuple xs) (Tuple ys) = and $ (length xs == length ys) : zipWith (==) xs ys 
    (==) (For a) (For b) = a == b
    (==) _ _ = False

instance Functor List where 
    fmap f (Unit a) = Unit (f a) 
    fmap f (Tuple xs) = Tuple (map (fmap f) xs)
    fmap f (For arr)  = For (fmap (fmap f) arr)

instance Fold.Foldable List where
    foldr f id (Unit a)   = f a id 
    foldr f id (Tuple xs) = foldr (flip $ Fold.foldr f) id xs 
    foldr f id (For arr)  = Fold.foldr (flip $ Fold.foldr f) id  arr

size :: List a -> Int 
size (Tuple xs)  = sum (map size xs)
size (Unit _) = 1
size (For arr) = len arr * size (arr ! 0)

sizeArr :: Arr (List a) -> Int 
sizeArr arr = len arr * size (arr ! 0 )
--------------------------------------------------------------------------------

type FData = List (DExp, Type)

--------------------------------------------------------------------------------

class Flatten a where 
    toFData :: a -> FData
    fromFData :: FData -> a 

instance Flatten FData where 
    toFData = id
    fromFData = id 

instance Flatten IntE where 
    toFData a = Unit (unE a, Int)
    fromFData (Unit (a,Int)) = E a

instance Flatten FloatE where 
    toFData a = Unit (unE a, Float)
    fromFData (Unit (a,Float)) = E a

instance Flatten WordE where 
    toFData a = Unit (unE a, Unsigned Int)
    fromFData (Unit (a,Unsigned Int)) = E a
    
instance (Flatten a, Flatten b) => Flatten (a,b) where 
    toFData (x,y) = Tuple [toFData x,toFData y]
    fromFData (Tuple [a,b]) = (fromFData a,fromFData b)

instance (Flatten a, Flatten b, Flatten c) => Flatten (a,b,c) where 
    toFData (x,y,z) = Tuple [toFData x,toFData y,toFData z]
    fromFData (Tuple [x,y,z]) = (fromFData x,fromFData y, fromFData z)

instance Flatten a => Flatten (Arr a) where 
    toFData a = For (fmap toFData a)
    fromFData (For a) = 
        fmap fromFData a

--------------------------------------------------------------------------------
