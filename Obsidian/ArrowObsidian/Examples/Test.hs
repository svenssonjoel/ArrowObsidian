{-# LANGUAGE TypeOperators #-}

{- 
   Test.hs
  
   A collection of examples

   Joel Svensson 

-}

module Obsidian.ArrowObsidian.Examples.Test where 

import Obsidian.ArrowObsidian
import Obsidian.ArrowObsidian.PureAPI

import Prelude hiding (foldr,zipWith)


import Data.Foldable
import Data.Bits

{- 
   Example: execute saxpy
   
   *Test> execute (saxpyKernel 2.0) ((replicate 4 1.1) ++ (replicate 4 1.0) :: [FloatE])
   [3.2,3.2,3.2,3.2]

   
   Example: execute sklansky
   *Test> execute (sklansky 3 (+)) ([0..7 :: IntE])
   [0,1,3,6,10,15,21,28]


   If you write programs, please send them to me ;) 


   Example: Get the kernel "text" 
   
   #1 create suitable input array
     let input = (Arr undefined 8 :: Arr FloatE)

   #2 use genKernel 
     putStrLn $ genKernel "saxpy" (saxpyKernel 2.0)  input
   
 
   Example2 Sklansky: 
   putStrLn $ genKernel "sklansky" (sklansky 3 (+)) (Arr (\ix -> index (variable "input") ix Int) 8 :: Arr IntE)
   
   
-} 


--------------------------------------------------------------------------------
--  Permutations 
--------------------------------------------------------------------------------
coalesce2 :: Choice a => (Arr a -> Arr a)
coalesce2 = unriffle 

--coalesce4 = 



--------------------------------------------------------------------------------
-- Pairwrapper
--------------------------------------------------------------------------------
pairWrapper p = pure halve ->- p 


--------------------------------------------------------------------------------
-- Sklansky
--------------------------------------------------------------------------------
sklansky :: (Flatten a, Choice a) => 
             Int -> (a -> a -> a) -> (Arr a :-> Arr a) 
sklansky 0 op = pure id
sklansky n op = two (sklansky (n-1) op) ->- pure (fan op) 
                   ->- sync

fan op arr = conc (a1, (fmap (op c) a2)) 
    where (a1,a2) = halve arr
          c       = a1 ! (fromIntegral (len a1 - 1))


-- Sklansky that writes blockmaximums to a special array 
sklansky2 :: (Flatten a, Choice a) => 
             Int -> (a -> a -> a) -> (Arr a :-> (Arr a,Arr a))
sklansky2 n op = sklansky n op ->- pure out
    where 
      out arr = (arr,singleton (arr ! (n-1)))
          where n = fromIntegral $ len arr 

-- distribute blockmaximums over arrays 
distribute :: (a -> a -> a) -> ((Arr a, Arr a) :-> (Arr a))
distribute op = pure distr
    where 
      distr (a1,a2) = fmap (op (a1 ! 0)) a2


-------------------------------------------------------------------------------- 
-- ODD-EVEN Sort 
--------------------------------------------------------------------------------

mergeOE :: (Choice a, Flatten a) => Int -> ((a,a) -> (a,a)) -> (Arr a :-> Arr a)
mergeOE 1 f = pure (unpair .  fmap f  . pair)
mergeOE n f = ilv (mergeOE (n-1) f) ->- sync ->- 
                pure (odds f)
sortOE :: Int -> (Arr IntE :-> Arr IntE)
sortOE  0 =  pure id
sortOE  n =  two (sortOE (n-1)) ->- sync ->- mergeOE n cmp 

-------------------------------------------------------------------------------- 
-- Bitonic Sort  
--------------------------------------------------------------------------------

bfly :: (Choice a, Flatten a) => 
        Int -> ((a,a) -> (a,a)) -> (Arr a :-> Arr a)
bfly 0 f = pure id 
bfly n f = ilv (bfly (n-1) f) ->- sync ->- pure (evens f) 

sortB :: Int -> (Arr IntE :-> Arr IntE)
sortB 0 = pure id
sortB n = two (sortB (n-1)) ->- pure reverseHalf ->- 
              sync ->- bfly n cmp
          where reverseHalf arr = let (a1,a2) = halve arr 
                                  in  conc (a1,rev a2)


-------------------------------------------------------------------------------- 
-- Comparator etc
-------------------------------------------------------------------------------- 
cmp :: (Ordered a, Choice a) => (a, a) -> (a, a)
cmp (a,b) = ifThenElse (a <* b)  (a,b) (b,a)

apa :: Arr IntE :-> Arr IntE
apa = pure (evens2 cmp) ->- sync

bepa :: Arr IntE :-> Arr IntE
bepa = pure (odds2 cmp) ->- sync

cepa :: Arr IntE :-> Arr IntE
cepa = ilv (pure (evens cmp)) ->- sync

-------------------------------------------------------------------------------- 
-- Summing stuff 
--------------------------------------------------------------------------------

sumUp :: Int -> Arr IntE :-> Arr IntE
sumUp 0 = pure id 
sumUp n = pure (pairwise (+)) ->- sync 
                  ->- sumUp (n-1)


sumUp2 :: Int -> Arr IntE :-> Arr IntE
sumUp2 0 = pure id 
sumUp2 n = pure (pairwise (+))
                  ->- sumUp2 (n-1)

-------------------------------------------------------------------------------- 
-- Peculiar way to get the minimum
--------------------------------------------------------------------------------
minimum' :: Int -> Arr IntE :->  Arr IntE 
minimum' 0 = pure id 
minimum' n = two (minimum' (n-1)) ->- pure smallestOfTwo ->- sync

smallestOfTwo :: Arr IntE -> Arr IntE 
smallestOfTwo arr 
    | len arr /= 2 = error "wrong input"
    | otherwise =  singleton $ ifThenElse (a <* b) a b
    where 
      a = arr ! 0 
      b = arr ! 1  

-------------------------------------------------------------------------------- 
-- Permutations 
-------------------------------------------------------------------------------- 
riffle :: Choice a => Arr a -> Arr a 
riffle = conc . unzipp . pair 

unriffle :: Choice a => Arr a -> Arr a 
unriffle = unpair . zipp . halve 

-------------------------------------------------------------------------------- 
-- Sum and general reduce 
-------------------------------------------------------------------------------- 

mySum5 :: Arr IntE :-> Arr IntE 
mySum5 = pure (chopN 4) ->- sync ->- 
         pure (fmap (foldr (+) 0)) ->- sync ->- 
         pure (singleton . (foldr (+) 0))


reduce :: Flatten a => Int -> (a -> a -> a) -> Arr a :-> Arr a
reduce 0 f = pure id 
reduce n f = pure op ->- sync ->- reduce (n-1) f 
    where 
      op = fmap (uncurry f) . pair


reduce' :: Flatten a => Int -> (a -> a -> a) -> Arr a :-> Arr a
reduce' 0 f = pure id 
reduce' n f = pure halve ->- zipWith (uncurry f) ->- sync ->- reduce' (n-1) f 
  


-------------------------------------------------------------------------------- 
-- Vector Add
-------------------------------------------------------------------------------- 

vecPlus (x,y,z) (x1,y1,z1) = (x+x1,y+y1,z+z1)

type Vec3 a = (a,a,a) 

vecAdd :: Arr (Vec3 FloatE,Vec3 FloatE) :-> Arr (Vec3 FloatE) 
vecAdd = pure (fmap (uncurry vecPlus))
 
-------------------------------------------------------------------------------- 
-- Replace occurances of i
-------------------------------------------------------------------------------- 
replace :: (Equal a, Choice a) => 
           a -> (Arr a, Arr a) -> Arr a
replace i = fmap f . zipp
  where 
    f (a,b) = (a ==* i) ?? (b,a) 

replaceK :: (Equal a, Choice a) => 
                 a -> (Arr a, Arr a) :-> Arr a
replaceK i = pure  $ replace i


-------------------------------------------------------------------------------- 
-- SAXPY
-------------------------------------------------------------------------------- 

zipWith f  =  pure (fmap f . zipp)



saxpy :: (Choice a, Num a) => a -> (Arr a, Arr a) :-> Arr a
saxpy alpha = pure (\(a1,a2) -> (coalesce2 a1, coalesce2 a2)) ->- 
                 zipWith (\(x, y) -> alpha * x + y) ->- pure riffle  


saxpy'' :: (Choice a, Num a) => a -> (Arr a, Arr a) :-> Arr (a,a)
saxpy'' alpha = pure (\(a1,a2) -> (unriffle' a1, unriffle' a2)) ->- 
                 zipWith (\(x, y) -> alpha * x + y) ->- pure riffle' ->- pure pair 



saxpy' :: (Choice a, Num a) => a -> (Arr a, Arr a) :-> Arr (a,a)
saxpy' alpha = pure (\(a1,a2) -> (coalesce2 a1, coalesce2 a2)) ->- 
               zipWith (\(x, y) -> alpha * x + y) ->-  pure riffle ->- pure pair   


saxpy2 :: Num a => a -> (Arr a, Arr a) :-> Arr a
saxpy2 alpha = zipWith (\(x, y) -> alpha * x + y)


saxpyApa :: Num a => a -> (Arr a, Arr a) :-> Arr (Arr a)
saxpyApa alpha = pure (\(a,b) -> (chopN 4 a,chopN 4 b)) ->-
                  pure zipp ->- pure (fmap zipp) ->- 
                     pure (fmap (\arr -> fmap (\(x,y) -> alpha * x + y) arr)) --  ->- pure unChop

saxpyKernel alpha = (pairWrapper (saxpy alpha))
saxpyKernel' alpha = (pairWrapper (saxpy' alpha))
saxpyKernel'' alpha = (pairWrapper (saxpy'' alpha))
saxpyKernel2 alpha = (pairWrapper (saxpy2 alpha))
saxpyKernelApa alpha = (pairWrapper (saxpyApa alpha))


-------------------------------------------------------------------------------- 
-- Something with pairing
-------------------------------------------------------------------------------- 
apa' :: Arr IntE :-> Arr (IntE,IntE) 
apa' = pure pair ->- sync 

-------------------------------------------------------------------------------- 
-- Bunch Of Syncs 
-------------------------------------------------------------------------------- 
syncs :: Arr IntE :-> Arr IntE 
syncs = sync ->- sync ->- sync ->- sync 


-------------------------------------------------------------------------------- 
-- Dot Product
-------------------------------------------------------------------------------- 

-- zipwith mul 


mulKern :: (Arr FloatE, Arr FloatE) :-> Arr FloatE 
mulKern = zipWith (uncurry (*))

mulTest :: Arr FloatE :-> Arr FloatE
mulTest = pairWrapper mulKern

-- *Test> putStrLn $ genKernel "dotprod" (dotProdKern1 7) (Arr undefined  128,Arr undefined 128) True

-- As many threads as elements
dotProdKern1 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProdKern1 n = mulKern ->- sync ->- reduce' n (+) 

-- Half as many threads as elements 
dotProdKern2 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProdKern2 n = mulKern ->- reduce n (+) 

-- One fourth as many threads as elements
dotProdKern3 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProdKern3 n = mulKern ->- pure (pairwise (+)) ->- reduce (n-1) (+) 

 
--Combine last add with write.
dotProdKern4 :: Int -> (Arr FloatE, Arr FloatE) :-> Arr FloatE 
dotProdKern4 n = mulKern ->- pure (pairwise (+)) ->- reduce (n-2) (+)  ->- 
                   pure (pairwise (+))


dotProdTest :: Int -> Arr FloatE :-> Arr FloatE
dotProdTest n = pairWrapper (mulKern ->- reduce n (+)) 


-------------------------------------------------------------------------------- 
-- Extremely simple
-------------------------------------------------------------------------------- 

incrAll :: Arr IntE :-> Arr IntE 
incrAll = pure (fmap (+1))



incrRev :: Arr IntE :-> Arr IntE 
incrRev = incrAll ->- pure rev


revHalves :: Arr IntE :-> Arr IntE 
revHalves = two (pure rev) 


