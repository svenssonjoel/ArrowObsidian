{-# LANGUAGE GADTs,
             TypeOperators#-} 

{- 
   Core.hs, Main datastructures
   Joel Svensson
-}

module Obsidian.ArrowObsidian.Core where 

import Obsidian.ArrowObsidian.Flatten 

import Obsidian.ArrowObsidian.Arr 
import Obsidian.ArrowObsidian.Exp

import Text.Show.Functions

--------------------------------------------------------------------------------

{-
data a :-> b 
    = Pure (a -> b)
    | First (a :-> b) 
    | Sync (a -> Arr FData) (Arr FData :-> b) 
    | PSync (IndexE -> IntE) (a -> Arr FData) (Arr FData :-> b) 
      deriving Show
-}

data a :-> b where 
    Pure  :: (a -> b) -> (a :-> b) 
    Sync  :: (a -> Arr FData) -> (Arr FData :-> b) -> (a :-> b)

--------------------------------------------------------------------------------

runS :: a :-> b -> a -> b
runS (Pure f) a = f a  
runS (Sync f g) a = let a' = f a in runS g a'





--------------------------------------------------------------------------------


