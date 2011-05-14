{- 
   Printing.hs, Ugly prints expressions. 
   Joel Svensson.
   
   Todo: 
     pretty print ? 

-}

module Obsidian.ArrowObsidian.Printing where 

import Obsidian.ArrowObsidian.Exp 

import Numeric
import Data.Word

{- PRINTING OF DEXPS -}

pprint (LitInt a) = show a
pprint (LitUInt a) = show a
pprint (LitFloat a) = show a++"f"
pprint (LitBool True) = "1"
pprint (LitBool False) = "0"
pprint (Op2 Atan2 b c) = "atan2(" ++ pprint b ++ "," ++ pprint c ++ ")"
pprint (Op2 Pow b c) = "pow(" ++ pprint b ++ "," ++ pprint c ++ ")"
--pprint (Op2 Powi b c) = " (int)(pow((float)" ++ pprint b ++ "," ++ pprint c ++ "))"
pprint (Op2 Min  a b) = "min (" ++ a' ++ "," ++ b' ++ ")"
       where a' = pprint a
             b' = pprint b
pprint (Op2 Max  a b) = "max (" ++ a' ++ "," ++ b' ++ ")"
       where a' = pprint a
             b' = pprint b
pprint (Op2 a b c) 
    | phex a    = "(" ++ (pprintH b) ++ (printOp2 a) ++ (pprintH c) ++ ")"
    | otherwise = "(" ++ (pprint b) ++ (printOp2 a) ++ (pprint c) ++ ")"
pprint (Op1 a b) = "(" ++ printOp1 a b ++ ")"
pprint (If a b c ) = "(" ++  pprint a ++ " ?\n" ++ pprint b ++ " : \n" ++ pprint c ++ ")"
--pprint (Variable str ) = str
pprint (Index a [] _) = a
pprint (Index a [b] t) = 
    case t of 
      Int   -> "ix_int("++ a ++ ","++ pprint b ++")"
      Float -> "ix_float("++ a ++ "," ++ pprint b ++ ")"
      _     -> error "pprint: type not implemented"


phex BitwiseXor = True
phex BitwiseAnd = True
phex BitwiseOr  = True 
phex _ = False


pprintH (LitUInt a) = "0x" ++ showHex ((fromIntegral  a) :: Word) ""
pprintH (LitInt a) = "0x" ++ showHex ((fromIntegral  a) :: Word) ""
pprintH a = pprint a 




printOp1 BitwiseComp a = "~" ++ pprint a
printOp1 Not a   = "!" ++ pprint a
printOp1 Exp a   = " exp(" ++ pprint a ++ ")"
printOp1 Log a   = " log(" ++ pprint a ++ ")" 
printOp1 Log2i a = " ((int)log2f(" ++ pprint a ++ "))"
printOp1 Sin a   = " sin(" ++ pprint a ++ ")"
printOp1 Sqrt a  = " sqrt(" ++ pprint a ++ ")"
printOp1 Cos a   = " cos(" ++ pprint a ++ ")"
printOp1 Tan a   = " tan(" ++ pprint a ++ ")"
printOp1 ASin a  = " asin(" ++ pprint a ++ ")"
printOp1 ACos a  = " acos(" ++ pprint a ++ ")"
printOp1 ATan a  = " atan(" ++ pprint a ++ ")"
printOp1 ASinH a = " asinh(" ++ pprint a ++ ")"
printOp1 ACosH a = " acosh(" ++ pprint a ++ ")"
printOp1 ATanH a = " atanh(" ++ pprint a ++ ")"
-- Type casting 
printOp1 IntCastInt     a = "(int)(" ++ pprint a ++ ")"
printOp1 UIntCastInt    a = "(int)(" ++ pprint a ++ ")"
printOp1 FloatCastInt   a = "(int)(" ++ pprint a ++ ")"
printOp1 IntCastUInt    a = "(unsigned int)(" ++ pprint a ++ ")"
printOp1 UIntCastUInt   a = "(unsigned int)(" ++ pprint a ++ ")"
printOp1 FloatCastUInt  a = "(unsigned int)(" ++ pprint a ++ ")"
printOp1 IntCastFloat   a = "(float)(" ++ pprint a ++ ")"
printOp1 UIntCastFloat  a = "(float)(" ++ pprint a ++ ")"
printOp1 FloatCastFloat a = "(float)(" ++ pprint a ++ ")"


printOp2 Add = " + "
printOp2 Sub = " - "
printOp2 Div = " / "
printOp2 Mul = " * " 
printOp2 Mod = " % "
printOp2 And = " && "
printOp2 Or  = " || "
printOp2 Lt  = " < "
printOp2 Gt  = " > "
printOp2 Leq = " <= "
printOp2 Geq = " >= "
printOp2 Eq  = " == "
printOp2 Shl = " << "
printOp2 Shr = " >> "


printOp2 BitwiseXor = " ^ "
printOp2 BitwiseAnd = " & "
printOp2 BitwiseOr  = " | " 

