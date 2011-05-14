{- 
   Arr.hs, the Array representation used in Obsidian. 
   Joel Svensson 


-}
module Obsidian.ArrowObsidian.Arr where

import Obsidian.ArrowObsidian.Exp

--------------------------------------------------------------------------------

data Arr a = Arr (IndexE -> a) Int -- Dynamic content, Static length 
       

    
instance Show a => Show (Arr a) where 
    show (Arr f n) = "(Arr" ++ show2 f ++ show n ++ ")"
        where 
          show2 f = show [f i| i <- [0..1]]

                     
instance Functor Arr where
    fmap f (Arr ixf n) = Arr (\ix -> f (ixf ix)) n

instance Eq a => Eq (Arr a) where 
    (==) a1 a2 = len a1 == len a2 && 
                 (a1 ! variable "X") == 
                 (a2 ! variable "X")  


(!) (Arr f _) ix   = f ix

 
len (Arr _ n)    = n 

mkArr f n        = Arr f n 


singleton = rep 1 

rep n x = Arr (\ix -> x) n

--------------------------------------------------------------------------------
