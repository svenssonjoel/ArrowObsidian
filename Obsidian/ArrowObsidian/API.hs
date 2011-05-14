{-# LANGUAGE TypeOperators #-}

{-
  API.hs , building blocks that needs to know about the program 
           datastructure (a :-> b) 
  Joel Svensson

  

-}


module Obsidian.ArrowObsidian.API (
            (->-),
            pure, 
            sync,
            two,                
            ilv
           ) where 

import Data.Bits
import Data.Word
import Obsidian.ArrowObsidian.Arr
import Obsidian.ArrowObsidian.Core
import Obsidian.ArrowObsidian.Flatten 
import Obsidian.ArrowObsidian.Exp 
import Obsidian.ArrowObsidian.Bitwise
import Obsidian.ArrowObsidian.PureAPI 

--------------------------------------------------------------------------------
pure = Pure

(->-) :: (a :-> b) -> (b :-> c) -> (a :-> c)
Pure  f ->- Pure g   = Pure (g . f)
Pure  f ->- Sync g h = Sync (g . f) h
Sync  f h1 ->- h2    = Sync f (h1 ->- h2)

sync :: Flatten a => (Arr a :-> Arr a)
sync = Sync (fmap toFData) (pure (fmap fromFData))

--------------------------------------------------------------------------------

ilv f = Pure riffle' ->- two f ->- Pure unriffle'

--------------------------------------------------------------------------------

logInt n | n <= 1 = 0
logInt n = 1 + logInt (n `div` 2) 

--------------------------------------------------------------------------------

two :: (Arr a :-> Arr b) -> (Arr a :-> Arr b)
two (Pure f)  = Pure $ twoFF f
two (Sync f g) = Sync (twoFF f) (two g)


twoFF :: (Arr a -> Arr b) -> Arr a -> Arr b
twoFF f arr = 
    Arr (\i -> f (
          Arr (\j -> arr ! ((sh bit bit2 (i .&. num2)) .|. j)) n2) ! 
                                    (i .&. mask)) nl
    where 
      n2       = (len arr) `div` 2 :: Int 
      bit      = logInt n2 

      bit2     = logInt nl2
      num2     = fromIntegral $ 2^bit2
      mask     = complement num2

      nl      = 2 * nl2 
      nl2     = (len (f (Arr (\j -> arr ! variable "X") n2 )))

sh :: (Bits a) => Int -> Int -> a -> a
sh b1 b2 a | b1 == b2 = a  
           | b1 <  b2 = a `shiftR` (b2 - b1)
           | b1 >  b2 = a `shiftL` (b1 - b2)


twoFF' :: (Arr a -> Arr b) -> Arr a -> Arr b
twoFF' f arr =
    Arr (\i -> f (
            Arr (\j -> arr ! (ifThenElse (top i) (j + fromIntegral m) j)) m) ! (bot i)) nl
    where     
      m = len arr `div` 2

      top i = i >=* (fromIntegral m)
              
      bot i = i `mod` (fromIntegral m)
              
      nl = len arr 