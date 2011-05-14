
{-
  Bitwise.hs, bitwise operations. 
   There are many other bitwise operations implemented
   in for example Exp.hs that should probably be moved here. 

  Joel Svensson

  Last Edited: 13 Mar 2009 (Friday the 13th) 
-}

module Obsidian.ArrowObsidian.Bitwise where 


import Obsidian.ArrowObsidian.Exp 
import Data.Bits

-- rotLocalL rotates a subword 1 bit left
rotLocalL :: Bits a => a -> Int -> a 
rotLocalL a bits= (a `shiftR` 1) .|. (b `shiftL` (bits - 1))
    where 
      b   = (a .&. 1) -- is bit 1 set ?


rotLocalR :: Bits a => a -> Int -> a
rotLocalR a bits = (a') .|. (b `shiftR` (bits - 1))
    where 
      b   = a .&. (fromIntegral (shiftL (1::Int) (bits - 1)))
      a'  = (a `shiftL` 1) .&. max
      max = fromIntegral ((2^bits) - 1)


      -- bit = fromIntegral (2^(bits-1))
      -- bit = fromIntegral (2^(bits-1))


--------------------------------------------------------------------------------
{- Unsigned Integers -}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
{- signed Integers -}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
{- signed/unsigned Integers -}
--------------------------------------------------------------------------------

