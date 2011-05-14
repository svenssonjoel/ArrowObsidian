
{- 
   Joel Svensson 
   2010
 
-}

module DotProd where

import Obsidian

import Prelude hiding (foldr,zipWith)
import PureAPI

import Data.Foldable
import Data.Bits


-------------------------------------------------------------------------------- 
-- DotProducts
-------------------------------------------------------------------------------- 


zipWith f  =  pure (fmap f . zipp)


mult :: Num a  => (Arr a, Arr a) :-> Arr a
mult = zipWith $ uncurry (*)

reduce :: Flatten a => Int -> (a -> a -> a) -> Arr a :-> Arr a
reduce 0 f = pure id 
reduce n f = pure op ->- sync ->- reduce (n-1) f 
    where 
      op = fmap (uncurry f) . pair


reduce' :: Flatten a => Int -> (a -> a -> a) -> Arr a :-> Arr a
reduce' 0 f = pure id 
reduce' n f = pure halve ->- zipWith (uncurry f) ->- sync ->- reduce' (n-1) f 



dotProd :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProd n = mult ->- sync ->- reduce n (+) 

dotProd2 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProd2 n = mult ->-  reduce n (+) 

dotProd3 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProd3 n = mult ->- add_once ->- reduce (n-1) (+) 
    where add_once = pure (fmap (uncurry (+)) . pair) 

-------------------------------------------------------------------------------- 
-- wrapper
-------------------------------------------------------------------------------- 

wrapper prg = pure halve ->- prg
