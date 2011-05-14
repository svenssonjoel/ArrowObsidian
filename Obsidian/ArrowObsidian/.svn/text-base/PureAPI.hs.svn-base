{- 
   PureAPI.hs, building blocks
   
   Joel Svensson 

-}
module Obsidian.ArrowObsidian.PureAPI where 

import Obsidian.ArrowObsidian.Exp
import Obsidian.ArrowObsidian.Arr
import Obsidian.ArrowObsidian.Bitwise

import Data.Bits

import Data.Foldable
--------------------------------------------------------------------------------'
(??) b (x,y) = ifThenElse b x y 

intLog 1 = 0 
intLog n = 1 + (intLog (div n 2))

--------------------------------------------------------------------------------

unriffle' :: Arr a ->  Arr a 
unriffle' arr | even (len arr) = mkArr (\ix -> arr ! (rotLocalL ix bits) ) n
                   where 
                    n = len arr
                    bits = fromIntegral $ intLog n 
unriffle' _ = error "unriffle' demands even length"
              
riffle' :: Arr a ->  Arr a 
riffle' arr | even (len arr) = mkArr (\ix -> arr ! (rotLocalR ix bits) ) n
                  where 
                    n = len arr
                    bits = fromIntegral $ intLog n
riffle' _ = error "riffle' demands even length"
                  
instance Foldable Arr where 
    foldr f o arr = 
        Prelude.foldr f o [arr ! (fromIntegral ix) | ix <- [0.. (len arr - 1)]]

--------------------------------------------------------------------------------
flatten :: (Arr (Arr a)) -> Arr a 
flatten arrs | len (arrs ! 0) == 1 = 
                 mkArr (\ix -> (arrs ! ix) ! 0) m
                     where 
                       k = len arrs
                       n = len (arrs ! 0)
                       m = k * n
flatten arrs = mkArr (\ix -> (arrs ! (ix `div` fromIntegral k)) ! (ix `mod` fromIntegral n)) m
      where k = len arrs
            n = len (arrs ! 0)
            m = k*n


restruct :: Int -> Arr a ->  (Arr (Arr a)) 
restruct n arr = mkArr (\ix -> inner ix) n 
    where
      m       = len arr
      k       = m `div` n 
      inner a = mkArr (\ix -> arr  ! ((a * fromIntegral k) + ix) ) k

--------------------------------------------------------------------------------


chopN :: Int -> Arr a -> Arr (Arr a)
chopN n arr = 
    mkArr (\o -> (mkArr (\i -> arr ! ((o * (fromIntegral n)) + i)) n)) (fromIntegral m)
    where 
      m = (len arr) `div` n



nParts :: Int -> Arr a -> Arr (Arr a)
nParts n arr = mkArr (\o -> (mkArr (\i -> arr ! (((fromIntegral m) * o) + i)) m)) (fromIntegral n)
    where 
      m = (len arr) `div` n


unChop :: Arr (Arr a) -> Arr a 
unChop arr = mkArr (\ix -> let x = ix `mod` fromIntegral w
                               y = ix `div` fromIntegral w
                         in (arr ! y) ! x) newLen
    where
      h      = len arr
      w      = len (arr ! 0)
      newLen = h * w


--------------------------------------------------------------------------------

rev :: Arr a -> Arr a 
rev arr  = mkArr ixf n
    where 
        ixf ix = arr ! (fromIntegral (n-1) - ix)
        n = len arr 


--------------------------------------------------------------------------------

split :: Int -> Arr a -> (Arr a,Arr a)
split  m arr = 
    let n  = len arr
        h1 = mkArr (\ix -> arr ! ix)  m
        h2 = mkArr (\ix -> arr ! (ix + (fromIntegral m))) (n-m)
    in  (h1,h2)

halve :: Arr a -> (Arr a,Arr a)
halve arr = split (len arr `div` 2) arr

conc :: Choice a => (Arr a, Arr a) -> Arr a
conc (arr1,arr2) = 
    let (n,n') = (len arr1,len arr2)
    in mkArr (\ix -> ifThenElse (ix <* fromIntegral n) 
                                (arr1 !  ix)
                                (arr2 !  (ix - fromIntegral n))) (n+n')


--------------------------------------------------------------------------------

 
evens :: Choice a =>  ((a,a) -> (a,a)) -> Arr a -> Arr a
evens f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse ((mod ix 2) ==* 0)
                (ifThenElse ((ix + 1) <* (fromIntegral n))
                                (fst (f (arr ! ix,arr ! (ix + 1))))
                                (arr ! ix))
                (ifThenElse (ix <* (fromIntegral n))
                                (snd (f (arr ! (ix - 1),arr ! ix)))
                                (arr !  ix))) n

odds f arr = let (a1,a2) = split 1 arr 
             in  conc (a1,evens f a2)

--------------------------------------------------------------------------------
pair :: Arr a -> Arr (a,a) 
pair arr | odd (len arr) = error "Pair: Odd n"
         | otherwise = mkArr (\ix -> (arr ! (ix * 2),arr ! ((ix * 2) + 1))) nhalf
         where 
           n = len arr
           nhalf = div n 2

unpair :: Choice a => Arr (a,a) -> Arr a
unpair arr = 
    let n = len arr
    in  mkArr (\ix -> ifThenElse ((mod ix 2) ==* 0) 
                    (fst (arr ! (div ix 2)))
                    (snd (arr ! (div (ix-1) 2)))) (2*n)


pairwise :: (a -> a -> b) -> Arr a -> Arr b
pairwise f = fmap (uncurry f) . pair

--------------------------------------------------------------------------------

zipp :: (Arr a, Arr b) -> Arr (a,b)
zipp (arr1,arr2) = mkArr (\ix -> (arr1 ! ix,arr2 ! ix)) n
    where n = min (len arr1) (len arr2)

unzipp :: Arr (a,b) -> (Arr a, Arr b) 
unzipp arr =  (mkArr (\ix -> fst (arr ! ix)) n,
               mkArr (\ix -> snd (arr ! ix)) n)
    where n = len arr

--------------------------------------------------------------------------------

evens2 :: Choice a => ((a,a) -> (a,a)) -> Arr a -> Arr a
evens2 f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse ((ix `mod` 2) ==* 0)
                    (fst (f (arr ! ix,arr ! (ix + 1))))
                    (snd (f (arr ! (ix - 1),arr ! ix)))) n


odds2 :: Choice a => ((a,a) -> (a,a)) -> Arr a -> Arr a
odds2 f arr = 
    let n = len arr 
    in  mkArr (\ix -> ifThenElse (((ix - 1) `mod` 2) ==* 0)
                    (fst (f (arr ! ix,arr ! (ix + 1))))
                    (snd (f (arr ! (ix - 1),arr ! ix)))) n
                          

endS :: Int -> Arr a -> Arr a
endS n arr = mkArr (\ix -> arr ! (ix + (fromIntegral n)) ) nl
    where l = len arr 
          nl = l - n 
               

