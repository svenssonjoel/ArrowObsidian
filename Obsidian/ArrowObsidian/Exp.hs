{-# LANGUAGE TypeSynonymInstances, 
             FlexibleInstances, 
             FlexibleContexts, 
             UndecidableInstances #-}
            
{-
  Exp.hs, Loads of stuff in here 
  Joel Svensson

  Todo: 
    Clean up, some parts are not used, 
              some parts should be in other files.      

-}

module Obsidian.ArrowObsidian.Exp where 


import Obsidian.ArrowObsidian.Types

import Data.Bits
import Data.Word

import Numeric


data Type = Int | Bool | Float | DummyType
          | Unsigned Type
          | Shared_Array Type
          | Global_Array Type
          | Constant_Array Type
            deriving(Eq,Show,Read)
 

data DExp = LitInt  Int
          | LitUInt Word32
          | LitBool Bool
          | LitFloat Float 
          | Op2 Op2 DExp DExp 
          | Op1 Op1 DExp 
          | If DExp DExp DExp 
          -- | Variable Name
          | Index Name [DExp] Type    
            deriving(Eq,Show,Read)

data Op1 = Not
         | BitwiseComp 
         | Exp | Log | Sqrt
         | Log2i 
         | Cos | Sin | Tan
         | CosH | SinH | TanH
         | ACos | ASin | ATan
         | ACosH | ASinH | ATanH
         -- Type casting
         | IntCastFloat 
         | IntCastInt 
         | IntCastUInt
         | FloatCastFloat 
         | FloatCastInt 
         | FloatCastUInt
         | UIntCastFloat 
         | UIntCastInt 
         | UIntCastUInt


           deriving(Eq,Show,Read)


data Op2 = Add | Sub | Div | Mul | Mod | Pow 
      --   | Powi -- unsure.. (why did I put this here?)
         | And | Or 
         | BitwiseAnd | BitwiseOr | BitwiseXor
         | Lt | Leq | Gt | Geq | Eq 
         | Shl | Shr 
         | Min | Max
         | Atan2 
           deriving(Eq,Show,Read)

data  Exp a = E DExp 
              deriving(Eq)

instance Show (Exp a) where 
    show (E (LitUInt a))  = show a
    show (E (LitInt a))   = show a  
    show (E (LitBool a))  = if a then "1" else "0" 
    show (E (LitFloat a)) = show a
    show (E a) = "E (" ++ show a ++ ")"


instance (Num a, Num (Exp a), Read a) => Read (Exp a) where

         readsPrec i str = map (\(x,y) -> (fromIntegral x, y)) (readsPrec i str)

-- easy to remember types for expressions
type IndexE  = WordE
type IntE    = Exp Int
type UIntE   = WordE
type FloatE  = Exp Float 
type BoolE   = Exp Bool
type WordE   = Exp Word 

 
{- LIFTS ETC-} 

typ1 f (E a) = E (f a)
typ2 f (E a) (E b) = E (f a b)
typ3 f (E a) (E b) (E c) = E (f a b c)

unE (E a) = a

{- CONVERSIONS -}


-- castInt = typ1 (Op1 CastInt)


{- INTEGER OPERATIONS -}
--powi :: Exp Int -> Exp Int -> Exp Int
--powi = typ2 (Op2 Powi)

--powi :: Exp Int -> Exp Int -> Exp Int
--powi a b = castInt ((castFloat a) ** (castFloat b))

log2i :: Exp Int -> Exp Int 
log2i = typ1 (Op1 Log2i)

powi :: Num a => Exp a -> Exp Int -> Exp a
powi = typ2 (Op2 Pow)

{- BitWise OPERATIONS -}



{- Logical ops -}
andi,ori :: Exp Bool -> Exp Bool -> Exp Bool
andi = typ2 (Op2 And)
ori  = typ2 (Op2 Or)

invi :: Exp Int -> Exp Int 
invi = typ1 (Op1 Not)

true_ = E (LitBool True)
false_ = E (LitBool False)


{- ARRAY OPERATIONS -}

index :: Name -> IndexE -> Type -> Exp a
index name ix t = E (Index name [unE ix] t)

-- Type causes issues. 
-- Look at where.. 
-- ( THIS IS HACKY!!! -> remove the Type field here) 
variable name  = E (Index name [] DummyType)


--index :: Exp a -> IndexE -> Type -> Exp a
--index  arr ix t = E (Index (unE arr) (unE ix) t)


--variable name  = E (Variable name)

{- Things with Boolean result -}
-- 8,9 taken out of thin air
infixl 8 <*,<=*,>*,>=* 
infixl 9 ==*


class Ordered  a where 
    (<*)  :: a -> a -> Exp Bool
    (<=*) :: a -> a -> Exp Bool
    (>*)  :: a -> a -> Exp Bool
    (>=*) :: a -> a -> Exp Bool
    
class Equal a where 
    (==*) :: a -> a -> Exp Bool


--(<*),(<=*),(>*),(>=*),(==*) :: Exp a -> Exp a -> Exp Bool
instance Ordered (Exp a) where 
    (<*) (E (LitInt a)) (E (LitInt b)) = E (LitBool (a < b))
    (<*) (E (LitUInt a)) (E (LitUInt b)) = E (LitBool (a < b))
    (<*) (E (LitFloat a)) (E (LitFloat b)) = E (LitBool (a < b))
    (<*)  a b = E (Op2 Lt (unE a) (unE b)) 

    (<=*) (E (LitInt a)) (E (LitInt b)) = E (LitBool (a <= b))
    (<=*) (E (LitUInt a)) (E (LitUInt b)) = E (LitBool (a <= b))
    (<=*) (E (LitFloat a)) (E (LitFloat b)) = E (LitBool (a <= b))
    (<=*) a b = E (Op2 Leq (unE a) (unE b))
                
    (>*) (E (LitInt a)) (E (LitInt b)) = E (LitBool (a > b))
    (>*) (E (LitUInt a)) (E (LitUInt b)) = E (LitBool (a > b))
    (>*) (E (LitFloat a)) (E (LitFloat b)) = E (LitBool (a > b))
    (>*)  a b = E (Op2 Gt (unE a) (unE b)) 

    (>=*) (E (LitInt a)) (E (LitInt b)) = E (LitBool (a >= b))
    (>=*) (E (LitUInt a)) (E (LitUInt b)) = E (LitBool (a >= b))
    (>=*) (E (LitFloat a)) (E (LitFloat b)) = E (LitBool (a >= b))
    (>=*) a b = E (Op2 Geq (unE a) (unE b))

instance Equal (Exp a) where
    (==*) (E (LitInt a)) (E (LitInt b)) = E (LitBool (a == b))
    (==*) (E (LitUInt a)) (E (LitUInt b)) = E (LitBool (a == b))
    (==*) (E (LitFloat a)) (E (LitFloat b)) = E (LitBool (a == b))
    (==*) a b = E (Op2 Eq (unE a) (unE b))


instance (Equal a, Equal b) => Equal (a,b) where 
    (==*) (a1,b1) (a2,b2) = (a1 ==* a2) `andi` (b1 ==* b2)

instance (Equal a, Equal b, Equal c) => Equal (a,b,c) where 
    (==*) (a1,b1,c1) (a2,b2,c2)
        = (a1 ==* a2) `andi` (b1 ==* b2) `andi` (c1 ==* c2)

    

neg :: Exp Bool -> Exp Bool
neg = typ1 (Op1 Not)

{- CLASSES RELATED TO EXP -} 

class Choice a where 
    ifThenElse :: Exp Bool -> a -> a -> a


--------------------------------------------------------------------------------
{- INSTANCE DECLARATIONS -}
--------------------------------------------------------------------------------

instance Num (Exp Int) where

    (+) (E (LitInt a)) (E (LitInt b)) = E (LitInt (a+b))
    (+) (E (LitInt 0)) x = x      
    (+) x (E (LitInt 0)) = x 
    (+) a@(E (Op2 Sub x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Add) a b
    
    (+) x y = typ2 (Op2 Add) x y 

    (-) (E (LitInt a)) (E (LitInt b)) = E (LitInt (a - b))
    (-) x (E (LitInt 0)) = x
    (-) a@(E (Op2 Add x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Sub)a b
    (-) x y = typ2 (Op2 Sub) x y 

    (*) (E (LitInt a)) (E (LitInt b)) = E (LitInt (a * b))
    (*) (E (LitInt 1)) a = a
    (*) a (E (LitInt 1)) = a
    (*) x y = typ2 (Op2 Mul) x y 

 


    negate a = 0 - a
    signum a = error "not implemented"
    abs a    = error "not implemented"
   
    fromInteger = (E . LitInt) . fromInteger . toInteger

instance Num UIntE where

    (+) (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a+b))
    (+) (E (LitUInt 0)) x = x      
    (+) x (E (LitUInt 0)) = x 
    (+) a@(E (Op2 Sub x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Add) a b
    (+) x y = typ2 (Op2 Add) x y 

    (-) (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a - b))
    (-) x (E (LitUInt 0)) = x
    (-) a@(E (Op2 Add x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Sub)a b

    (-) x y = typ2 (Op2 Sub) x y 

    (*) (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a * b))
    (*) a (E (LitUInt 2)) = a `shiftL` 1 
    (*) (E (LitUInt 2)) a = a `shiftL` 1 
    (*) a (E (LitUInt 4)) = a `shiftL` 2 
    (*) (E (LitUInt 4)) a = a `shiftL` 2 
    (*) (E (LitUInt 1)) a = a
    (*) a (E (LitUInt 1)) = a
    (*) x y = typ2 (Op2 Mul) x y 
 


    negate a = 0 - a
    signum a = error "not implemented"
    abs a    = error "not implemented"
    fromInteger = (E . LitUInt) . fromInteger . toInteger


instance Num (Exp Float) where 
    (+) (E (LitFloat a)) (E (LitFloat b)) = E (LitFloat (a+b))
    (+) (E (LitFloat 0.0)) x = x      
    (+) x (E (LitFloat 0.0)) = x 
    (+) a@(E (Op2 Sub x y)) b@(E z) = if y == z then  E x 
                                      else typ2 (Op2 Add) a b
    (+) x y = typ2 (Op2 Add) x y 

  
    (-) = typ2 (Op2 Sub)
    (*) (E (LitFloat x)) (E (LitFloat y)) = E (LitFloat (x * y))
    (*) x y = typ2 (Op2 Mul) x y
    negate a = 0.0 - a
    signum a = error "not implemented"
    abs a    = error "not implemented"
    fromInteger x = E (LitFloat (fromIntegral x))

instance Real IntE where 
    toRational = undefined 

instance Real UIntE where 
    toRational = undefined 


instance Ord (Exp Int) where 
    compare = undefined 
    (<)  = undefined
    (>=) = undefined
    (>)  = undefined 
    (<=) = undefined 
    max = typ2 (Op2 Max)
    min = typ2 (Op2 Min)

instance Ord UIntE where 
    compare = undefined 
    (<) = undefined
    (>=) = undefined
    (>)  = undefined 
    (<=) = undefined 
    max = typ2 (Op2 Max)
    min = typ2 (Op2 Min)


    
instance Integral IntE where 
    
    div (E (LitInt a)) (E (LitInt b)) = E (LitInt (div a b))
    div (E (Op2 Mul x y)) (E z) | y == z = E x 
                                | x == z = E y
    div a b = E (Op2 Div (unE a) (unE b))

    --- implement modulo 2 using &
    mod (E (LitInt a)) (E (LitInt b)) = E (LitInt (mod a b))
    mod a b = typ2 (Op2 Mod) a b

    quotRem = undefined
    toInteger = undefined

instance Integral WordE where 

    div (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (div a b))

    div (E (Op2 Mul x y)) (E z) | y == z = E x 
                                | x == z = E y
    div a (E (LitUInt 2)) = a `shiftR` 1 
    div a b = E (Op2 Div (unE a) (unE b))

    --- implement modulo 2 using &
    mod a (E (LitUInt 2)) = a .&. 1
    mod (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (mod a b))
    mod a b = typ2 (Op2 Mod) a b

    quotRem = undefined
    toInteger = undefined

   

instance Fractional (Exp Float) where
    (/) (E (LitFloat a)) (E (LitFloat b)) = E (LitFloat (a / b))
    (/) x y= typ2 (Op2 Div) x y
    recip a = 1.0 / a
    fromRational = (E . LitFloat) . fromRational

instance Floating (Exp Float) where 
    pi    = (E . LitFloat) pi
    exp   = typ1 (Op1 Exp)
    log   = typ1 (Op1 Log)
    sqrt  = typ1 (Op1 Sqrt)
    (**)  = typ2 (Op2 Pow)
    sin   = typ1 (Op1 Sin)
    cos   = typ1 (Op1 Cos)
    tan   = typ1 (Op1 Tan)
    sinh  = typ1 (Op1 SinH)
    cosh  = typ1 (Op1 CosH)
    tanh  = typ1 (Op1 TanH)
    asin  = typ1 (Op1 ASin)
    acos  = typ1 (Op1 ACos)
    atan  = typ1 (Op1 ATan)
    asinh = typ1 (Op1 ASinH)
    acosh = typ1 (Op1 ACosH)
    atanh = typ1 (Op1 ATanH)


instance Enum (IntE) where 
    succ (E (LitInt a)) =  E (LitInt ((toEnum . (+1) . fromEnum) a))
    succ _ = error "Enum"
    pred (E (LitInt a)) = E (LitInt ((toEnum . (subtract 1) . fromEnum) a)) 
    pred _ = error "Enum"
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x y z = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    fromEnum (E (LitInt a)) = a
    toEnum a = (E (LitInt a))

instance Enum (UIntE) where 
    succ (E (LitUInt a)) =  E (LitUInt ((toEnum . (+1) . fromEnum) a))
    succ _ = error "Enum"
    pred (E (LitUInt a)) = E (LitUInt ((toEnum . (subtract 1) . fromEnum) a)) 
    pred _ = error "Enum"
    enumFrom x       =  map toEnum [fromEnum x ..]
    enumFromTo x y   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x y z = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    fromEnum (E (LitUInt a)) = fromIntegral a
    toEnum a = (E (LitUInt (fromIntegral a)))

instance Enum (FloatE) where 
    succ (E (LitFloat a)) =  E (LitFloat (succ a))
    succ _ = error "Enum"
    pred (E (LitFloat a)) = E (LitFloat (pred a)) 
    pred _ = error "Enum"
    enumFrom (E (LitFloat x)) =  map toEnum [fromEnum x ..]
    enumFromTo (E (LitFloat x)) (E (LitFloat  y))   =  map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo (E (LitFloat x)) (E (LitFloat y)) (E (LitFloat z)) = 
                        map toEnum [fromEnum x, fromEnum y .. fromEnum z]
    fromEnum (E (LitFloat a)) = (fromInteger . truncate) a
    toEnum a = (E (LitFloat (toEnum a)))

--------------------------------------------------------------------------------
{- Bits  -} 
--------------------------------------------------------------------------------
--Minimal complete definition: .&., .|., xor, complement, 
--  (shift or (shiftL and shiftR)), 
--  (rotate or (rotateL and rotateR)), bitSize and isSigned. 


hexVal str = fst $ head $ readHex str



singlebit (E (LitInt n)) = or [n == 2^x | x <- [0..1024]]
singlebit (E (LitUInt n)) = or [n == 2^x | x <- [0..1024]]
singlebit _ = False


instance Bits UIntE where 
    (.&.) a b = andOptU a b -- typ2 (Op2 (BitwiseAnd)) 
   
                                                      
    (.|.) a b = orOptU a b -- typ2 (Op2 (BitwiseOr))
--    (.&.) = typ2 (Op2 (BitwiseAnd)) 
--    (.|.) = typ2 (Op2 (BitwiseOr))
    xor   = typ2 (Op2 (BitwiseXor))
--    complement = typ1 (Op1 (BitwiseComp)) -- compOptU 
    complement = compOptU 
    shiftL n 0 = n 
    shiftL (E (LitUInt a)) n = E (LitUInt (a `shiftL` n)) 
    shiftL (E (Op2 Shr a (LitInt n1))) n 
           | n == n1 = (E a) .&. (complement (fromIntegral (2^n-1)))
    shiftL (E (Op2 Shl a (LitInt n1))) n = ((E a) `shiftL` (n+n1))
    --shiftL (E (Op2 BitwiseOr a b)) n = ((E a) `shiftL` n) .|. ((E b) `shiftL` n)
    --shiftL (E (Op2 BitwiseAnd a b)) n = ((E a) `shiftL` n) .&. ((E b) `shiftL` n)
    shiftL a n = E (Op2 Shl (unE a) (LitInt n))
    shiftR n 0 = n
    shiftR (E (LitUInt a)) n = E (LitUInt (a `shiftR` n)) 
    --shiftR (E (Op2 BitwiseOr a b)) n = ((E a) `shiftR` n) .|. ((E b) `shiftR` n)
    --shiftR (E (Op2 BitwiseAnd a b)) n = ((E a) `shiftR` n) .&. ((E b) `shiftR` n)
    shiftR a n = E (Op2 Shr (unE a) (LitInt n)) 
    bitSize  _ = 32 
    isSigned _ = False
    

instance Bits IntE where 
    (.&.) = typ2 (Op2 BitwiseAnd) 
 --   (.&.) = (&*) -- typ2 (Op2 BitwiseAnd)
    (.|.) = typ2 (Op2 BitwiseOr) 
 --   (.|.) = (|*) -- typ2 (Op2 BitwiseOr)
    xor   = typ2 (Op2 BitwiseXor) -- (^*) -- 
    complement =  typ1 (Op1 (BitwiseComp)) -- compOpt
    shiftL a n = E (Op2 Shl (unE a) (LitInt n))
    shiftR a n = E (Op2 Shr (unE a) (LitInt n)) 
    bitSize  _ = 32 
    isSigned _ = True

andOptU :: UIntE -> UIntE -> UIntE           
andOptU (E (LitUInt 0)) a  = 0
andOptU a (E (LitUInt 0))  = 0
andOptU a (E (LitUInt (-1))) = a -- -1 = ffffffff
andOptU (E (LitUInt   (-1))) a = a -- -1 = ffffffff
andOptU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .&. b))

--Experiment ---------------------
andOptU (E (Op2 BitwiseOr (Op2 Shr a (LitInt n)) (Op2 Shl b (LitInt m)))) (E (LitUInt c))
    | a == b && n <= m && c <= (fromIntegral m) = (E (Op2 Shr a (LitInt n))) .&. (E (LitUInt c))

andOptU (E (Op2 BitwiseOr (Op2 Shr a (LitInt n)) (Op2 Shl (Op2 BitwiseAnd b _)  (LitInt m)))) (E (LitUInt c))
    | a == b && n <= m && c <= (fromIntegral m) = (E (Op2 Shr a (LitInt n))) .&. (E (LitUInt c))


andOptU (E (Op2 BitwiseOr _ (Op2 BitwiseAnd a b))) c 
    | (E b) == c = E (Op2 BitwiseAnd a b)


----------------------------------


andOptU (E (Op2 BitwiseOr a b)) (E (Op2 BitwiseOr c d)) 
    | a == c = (E a) .|. ((E b) .&. (E d))
    | a == d = (E a) .|. ((E b) .&. (E c)) 
    | b == c = (E b) .|. ((E a) .&. (E d))
    | b == d = (E b) .|. ((E a) .&. (E c))
    | otherwise = E $ Op2 BitwiseOr (Op2 BitwiseAnd a b) (Op2 BitwiseAnd c d)
andOptU (E (Op2 BitwiseAnd a b)) c = (E a) .&. ((E b) .&. c) 

-- OK ? 
andOptU (E (Op2 Shl a (LitInt n1))) x | singlebit x = 
                   ((E a) .&. (x `shiftR` n1)) `shiftL` n1
andOptU (E (Op2 Shr a (LitInt n1))) x | singlebit x = 
                    ((E a) .&. (x `shiftL` n1)) `shiftR` n1
    


andOptU a b =  typ2 (Op2 BitwiseAnd) a b

--bandALU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .|. b))
--bandALU (E (Op2 BitwiseOr a b)) c = (E a) `borALU` ((E b) .|. c)
--bandALU  a b = typ2 (Op2 BitwiseOr) a b

--bandARU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .|. b))
--bandARU c (E (Op2 BitwiseOr a b)) = (c .|. (E a)) `borARU` (E b) 
--bandARU  a b = typ2 (Op2 BitwiseOr) a b

-- Complement
compOptU  :: UIntE -> UIntE
compOptU (E (Op1 BitwiseComp a)) = E a 
compOptU (E (LitUInt a)) = (E (LitUInt (complement a)))
compOptU  a = typ1 (Op1 BitwiseComp) a

compOpt  :: IntE -> IntE
compOpt (E (Op1 BitwiseComp a)) = E a 
compOpt (E (LitInt a)) = (E (LitInt (complement a)))
compOpt  a = typ1 (Op1 BitwiseComp) a


--Or 
orOptU :: UIntE -> UIntE -> UIntE 
orOptU a (E (LitUInt 0)) = a
orOptU (E (LitUInt 0)) a = a
orOptU (E (Op2 BitwiseAnd a b)) (E (Op2 BitwiseAnd c d)) 
    | a == c = (E a) .&. ((E b) .|. (E d))
    | a == d = (E a) .&. ((E b) .|. (E c)) 
    | b == c = (E b) .&. ((E a) .|. (E d))
    | b == d = (E b) .&. ((E a) .|. (E c))
    | otherwise = E $ Op2 BitwiseOr (Op2 BitwiseAnd a b) (Op2 BitwiseAnd c d)
                  -- (unE ((E a) .&. (E b))) (unE ((E c) .&.  (E d)))--

orOptU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .|. b))
orOptU (E (Op2 BitwiseOr a b)) c = (E a) `borALU` ((E b) .|. c) 
orOptU c (E (Op2 BitwiseOr a b)) = (c .|. (E a)) `borARU` (E b) 
orOptU a b = typ2 (Op2 BitwiseOr) a b


borALU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .|. b))
borALU (E (Op2 BitwiseOr a b)) c = (E a) `borALU` ((E b) .|. c)
borALU  a b = typ2 (Op2 BitwiseOr) a b

borARU (E (LitUInt a)) (E (LitUInt b)) = E (LitUInt (a .|. b))
borARU c (E (Op2 BitwiseOr a b)) = (c .|. (E a)) `borARU` (E b) 
borARU  a b = typ2 (Op2 BitwiseOr) a b


    
{-




(^*) (E (LitInt a)) (E (LitInt b)) = E (LitInt (a `xor` b))
(^*) (E a) (E b) = E (Op2 BitwiseXor a b)

-} 


--------------------------------------------------------------------------------
{- Choice  -} 
--------------------------------------------------------------------------------


instance Choice (Exp a) where 
    ifThenElse (E (LitBool True)) e1 e2  = e1
    ifThenElse (E (LitBool False)) e1 e2  = e2
    ifThenElse b e1 e2 = typ3 If b e1 e2

instance Choice (Exp a,Exp b) where 
    ifThenElse a (b,b') (c,c') = (typ3 If a b c ,typ3 If a b' c')

instance Choice (Exp a,Exp b,Exp c) where 
    ifThenElse a (b,b',b'') (c,c',c'') = 
        ( typ3 If a b c, typ3 If a b' c', typ3 If a b'' c'')



caseof a [(v,x)]    = x
caseof a ((v,x):xs) = ifThenElse (a ==* v) x 
                        (caseof a xs)



class Cast a where 
    fromInt :: IntE -> a 
    fromWord :: WordE -> a
    fromFloat :: FloatE -> a 
    fromBool :: BoolE -> a 

instance Cast IntE  where 
    fromInt = id 
    fromWord = typ1 (Op1 UIntCastInt)
    fromFloat = typ1 (Op1 FloatCastInt)
    fromBool  = typ1 (Op1 IntCastInt)

instance Cast FloatE where 
    fromInt = typ1 (Op1 IntCastFloat)
    fromWord = typ1 (Op1 UIntCastFloat)
    fromFloat = id
    fromBool  = typ1 (Op1 IntCastFloat)
    

instance Cast WordE  where 
    fromInt = typ1 (Op1 IntCastUInt)
    fromWord = id
    fromFloat = typ1 (Op1 FloatCastUInt)
    fromBool  = typ1 (Op1 IntCastUInt)
