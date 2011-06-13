{-# Language TypeOperators,   
             FlexibleInstances,
             IncoherentInstances,
             OverlappingInstances #-}

{- 
   CodeGen, Generate Code. 

   Joel Svensson
   
   


   TODO:
    A better way may be to type_check the expression 
    and from that info decide what ix_XXXXX() to use.
   
    AOS - SOA. Maybe it is a better idea to unzipp arrays 
    of pairs into pairs of arrays before code generation 
     (See what happens to data access patterns 
      This may give you coalesced reads for free in some cases. ) 

-}

{- XXXXXXX ----------------------------------------------------------------------
  2010-02-05 
 
  Look into: C AST Datatype (probably very good idea) 

  Everything in here needs to be rewritten in a more clear and structured way. 
  (It is an awfull mess) 

  #Not likely to happen# Looks like a complete rewrite of this module is called for! 

  DONE: 
    #Partial# Generalise the code generator: ok for inputs/outputs  up to triples
    
    generate kernels that talk about "block indices" 
    
    compute mem needed and threads needed in one pass.
 
  TODO: 
    Make (name->BlockSize) mapping a Data.Map 
    
    new genKernel function that generates kernels that 
     take extra arguments (fullblock, n), per output array,  
     and generates code that performs bounds checks (ON WRITES!) 
     
    Bounds Checking is not entirely correct. See the 
     reduction example. The boundschecking is more 
     complicated than first expected. 

-----------------------------------------------------------------------------  -}

{- Latest  ---------------------------------------------------------------------
  2010-02-23

  # Removing bounds checking for now, Going to put it back 
     when I know how to.

-----------------------------------------------------------------------------  -}

module Obsidian.ArrowObsidian.CodeGen where 

import Obsidian.ArrowObsidian.Core
import Obsidian.ArrowObsidian.Exp 
import Obsidian.ArrowObsidian.Arr 
import Obsidian.ArrowObsidian.Printing
import Obsidian.ArrowObsidian.Flatten
import Obsidian.ArrowObsidian.Types

import Obsidian.ArrowObsidian.PureAPI

import Data.List

import Control.Monad.State.Lazy

--------------------------------------------------------------------------------
-- Code type
--------------------------------------------------------------------------------
type Code = String -- For now..

--------------------------------------------------------------------------------
-- Output
--------------------------------------------------------------------------------
incr = do n <- get; put (n + 1); return n

namedArray name n = Arr (\ix -> (Index name 
                                 [unE ix] undefined)) n  :: Arr DExp

class Output a where 
    output :: a -> State Int Code
    writeOutput :: a -> Int -> State Int Code 
    fixOffsets :: a -> [(Name,BlockSize)] -> a  

instance Output (Arr FData) where 
    fixOffsets arr nomSize = fmap (addOffset nomSize) arr 
    output _ = do n <- incr; return ("word* result" ++ show n {- ++ ",int n" ++ show n  -} ) 
    writeOutput arr nt  = 
        do 
          n <- incr
          let result = namedArray ("result" ++ show n) (sizeArr arr)
              wOffset = fromIntegral (sizeArr arr) * variable "blockIdx.x"
          return $ writeL (len arr) nt wOffset arr result 

instance Flatten a => Output (Arr a) where 
    fixOffsets arr nz = fmap fromFData (fixOffsets (fmap toFData arr) nz)
    output arr = output (fmap toFData arr)  
    writeOutput arr = writeOutput (fmap toFData arr) 

instance (Output a, Output b) => Output (a,b) where
    fixOffsets (a,b) ns = (fixOffsets a ns,fixOffsets b ns)
    output (a,b) = 
        do 
          a' <- output a
          b' <- output b 
          return $ a' ++ "," ++ b' 
    writeOutput (a,b) nt = 
        do 
          a' <- writeOutput a nt 
          b' <- writeOutput b nt 
          return $ a' ++ "\n" ++ b' 
  
instance (Output a, Output b, Output c) => Output (a,b,c) where
    fixOffsets (a,b,c) ns = (fixOffsets a ns, fixOffsets b ns, fixOffsets c ns)
    output (a,b,c) = 
        do 
          a' <- output a
          b' <- output b
          c' <- output c
          return $ a' ++ "," ++ b' ++ "," ++ c' 
    writeOutput (a,b,c) nt = 
        do 
          a' <- writeOutput a nt 
          b' <- writeOutput b nt 
          c' <- writeOutput c nt 
          return $ a' ++ "\n" ++ b' ++ "\n" ++ c'
  

--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------
type BlockSize = Int 

class Input a where 
    input :: a -> State Int (Code,[(Name,BlockSize)])
    toInput :: a -> State Int a 

instance Input (Arr FData) where 
    input arr = 
        do 
          n <- incr
          let nom = "input" ++ show n
          return ("word* "++ nom, [(nom,len arr)])
    toInput arr = 
        do 
          n <- incr 
          let nom = "input" ++ show n 
              sz  = sizeArr arr
              arr' = namedArray nom sz 
          return (readL (arr ! 0) arr')
              

instance Flatten a => Input (Arr a) where 
    input arr = input (fmap toFData arr)  
    toInput arr = 
        do 
          arr' <- toInput (fmap toFData arr)
          return $ fmap fromFData arr'

instance (Input a, Input b) => Input (a,b) where
    input (a,b) = 
        do 
          (a',m1) <- input a
          (b',m2) <- input b 
          
          return $ (a' ++ "," ++ b', m1 ++ m2)
    toInput (a,b) = 
        do 
          a' <- toInput a
          b' <- toInput b
          return (a',b')

instance (Input a, Input b, Input c) => Input (a,b,c) where
    input (a,b,c) = 
        do 
          (a',m1) <- input a
          (b',m2) <- input b
          (c',m3) <- input c
          return $ (a' ++ "," ++ b' ++ "," ++ c',m1 ++ m2 ++ m3)
    toInput (a,b,c) = 
        do 
          a' <- toInput a
          b' <- toInput b
          c' <- toInput c
          return (a',b',c')

   
--------------------------------------------------------------------------------
-- Current Code Generation Code 
--------------------------------------------------------------------------------

code :: (MemThreads b, Output b) => 
        Threads -> (a :-> b) -> a -> [(Name,BlockSize)] -> Code
code threads f a nomSize 
    = let (_,c,_) =  code' True threads f a True nomSize 
      in c
   

code' :: (MemThreads b, Output b) => 
         Bool -> Int -> (a :-> b) -> a -> Bool -> [(Name,BlockSize)] -> (b,Code,Bool)
code' first nt (Pure f) a sm1 nomSize 
    = (res, str, sm1)
    where   
      res  = f a --result to be written to result arrays. 
      res' = if first then (fixOffsets res nomSize) else res
      str = evalState (writeOutput res' nt) 0 

-- This case should be simpler. Since here we know 
-- that the data is a single array (from the type of Sync) 
code' first nt (Sync f g) a sm1 nomSize
    = (res,str ++ syncthreads ++  c,sm1'')
    where 
      ts     = len a'
      syncthreads = "__syncthreads();\n"
      a'     = f a 
      a''    = if first then (fixOffsets a' nomSize) else a'
      shape  = a' ! 0
      inp    = readL shape result
      str    = writeL ts nt 0 a'' result
      (res,c,sm1'')  = code' False nt g inp sm1' nomSize
      sm1'     = not sm1
      result = 
        Arr (\ix -> (Index (if sm1 then "sm1" else "sm2") [unE ix] undefined)) 
                (sizeArr a') :: Arr DExp

  
--------------------------------------------------------------------------------
-- Run a program. Gives: return shape, number of threads needed, shared mem req
--------------------------------------------------------------------------------
type Memory = Int
type Threads = Int 

runPrgMT :: MemThreads b => 
            a :-> b -> a -> ((Memory,Threads),b)
runPrgMT (Pure f) a = ((0,t),b) 
    where b = f a 
          (m,t) = memAndThreads b 
runPrgMT (Sync f g) a = ((max m m', max t t'),c) 
    where 
      ((m',t'),c) = runPrgMT g b
      (m,t) = memAndThreads b
      b = f a 

--------------------------------------------------------------------------------
-- Compute amount of memory needed and number of threads 
--------------------------------------------------------------------------------
class MemThreads a where 
    memAndThreads :: a -> (Memory,Threads)
   
instance MemThreads (Arr FData) where
    memAndThreads arr = (sizeArr arr, len arr) 

instance Flatten a => MemThreads (Arr a) where 
    memAndThreads arr = memAndThreads (fmap toFData arr)

instance (MemThreads a, MemThreads b) => MemThreads (a,b) where 
    memAndThreads (a,b) = let (m,t) =  memAndThreads a
                              (m',t') = memAndThreads b
                          in (m + m', t + t')

--------------------------------------------------------------------------------
-- Generate a Kernel 
--------------------------------------------------------------------------------
genKernel :: (MemThreads b, Input a, Output b) => 
             Name -> (a :-> b) -> a -> Code 
genKernel name f a = genKernelMT name f a b m t
    where 
      ((m,t),b) = runPrgMT f a 

--------------------------------------------------------------------------------
-- Given parameters, generate a kernel.
--------------------------------------------------------------------------------
genKernelMT :: (MemThreads b, Input a, Output b) => 
             Name -> (a :-> b) -> a -> b -> Memory -> Threads -> Code 

genKernelMT name f a b mem threads = 
    head ++ init ++ (if (mem > 0) then sm1 ++ sm2  else "") ++ body ++ "}\n"
    where
      a'      = evalState (toInput a) 0
      (inputs,nomSize)  = evalState (input a) 0
      outputs = evalState (output b) 0 
      head    = "__global__ void " ++ name ++ 
                "(" ++ inputs ++"," ++ outputs ++ "){\n" 
      init    = "unsigned int tid = (unsigned int)threadIdx.x;\n" 
      sm1     =  "extern __shared__ unsigned int s_data[];\n" ++
                 "word __attribute__((unused)) *sm1 = &s_data[0];\n"
      sm2     = "word __attribute__((unused)) *sm2 = &s_data[" ++ show mem ++ "];\n"
      body    = code threads f a' nomSize 

--inputApa1   = Arr (\ix -> index (variable "apa") ix Int) 8 :: Arr IntE 
--inputApa2   = Arr (\ix -> index (variable "apa") ix Int) 1 :: Arr IntE 
--inputShape1 = Arr (\ix -> index (variable "input0") ix Float) 64 :: Arr FloatE 
--inputShape2 = Arr (\ix -> index (variable "input1") ix Float) 64 :: Arr FloatE 
--outputShape1 = Arr (\ix -> index (variable "out0") ix Int) 8 :: Arr IntE 
--outputShape2 = Arr (\ix -> index (variable "out1") ix Int) 4 :: Arr IntE 
 

-- -----------------------------------------------------------------------------
--  genMakefile now uses env variables CUDA_INSTALL_PATH and
--  CUDA_SDK_PATH(not used at the moment, maybe remove)  
-- -----------------------------------------------------------------------------
genMakefile :: Name -> String 
genMakefile name = 
    "TARGET := " ++ name ++  "\nCOMMON_PATH := $(CUDA_SDK_PATH)/common\n\n\
    \LIBPATHS := -L$(CUDA_INSTALL_PATH)/lib -L$(COMMON_PATH)/lib -L$(CUDA_\
    \SDK_PATH)/lib\nINCPATHS := -I$(CUDA_INSTALL_PATH)/include -I$(COMMON_\
    \PATH)/inc\nLIBRARIES := -lcuda -lcudart\n\nLIBRARIESEMU :\
    \=-lcudart\n\nNVCC := nvcc \n\nall: $(TARGET)\n$(TARGET): \
    \$(TARGET).cu\n\t$(NVCC) -o $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPA\
    \THS) $(LIBRARIES)\n\nemulation: $(TARGET).cu\n\t$(NVCC) -deviceemu -o\
    \ $(TARGET) $(TARGET).cu $(INCPATHS) $(LIBPATHS) $(LIBRARIESEMU)\n\ncl\
    \ean:\n\trm $(TARGET)\n"

-- -----------------------------------------------------------------------------
-- readL and writeL both have strange names ;) 
-- -----------------------------------------------------------------------------
-- TODO: Improve names. 
readL :: FData -> Arr DExp -> Arr FData
readL shape ws = Arr (\ix -> shapify shape ws 0 ix) (len ws `div` s)
    where 
      s = size shape

writeL :: Int -> Int -> IndexE -> Arr FData -> Arr DExp -> Code 
writeL ts nt wOffset arr targ =  str                 
    where 
      str     = case compare ts nt of 
                  LT -> "if (tid < " ++ show ts ++ "){\n" ++ 
                        repr ++ "}\n"
                        
                  EQ -> repr
                  GT -> error $ "writeL: " ++ show ts ++ " " ++ show nt 
      repr    = (assignment  0 (arr ! (variable "tid")) 
                 (shapify (arr ! 0) targ wOffset (variable "tid")))


-- -----------------------------------------------------------------------------
-- assignment 
-- -----------------------------------------------------------------------------
assignment :: Int -> FData -> FData -> Code 
assignment _ (Unit (a,t1)) (Unit (b,t2)) =
        pprint b ++ 
        " = " ++ 
        pprint a ++ ";\n" 
      
assignment i (Tuple [x1,x2]) (Tuple [t1,t2]) = 
    assignment  i x1 t1 ++ assignment i x2 t2
assignment i (Tuple [x1,x2,x3])  (Tuple [t1,t2,t3]) = 
    assignment i x1 t1 ++ assignment i x2 t2 ++ assignment i x3 t3
assignment i (For x1) (For t1) = 
    "for (int i" ++ show i ++" = 0; i"++ show i ++ " < " ++ show n ++ "; i" ++ show i ++ " ++){\n" ++ 
                 assignment (i+1) (x1 ! (variable ("i" ++ show i))) 
                                     (t1 ! (variable ("i" ++ show i))) ++ 
    "}\n"
    
    where n = len x1
    
assignment _ _ _ =  error "assignment: Both arguments need to have same shape"


-- -----------------------------------------------------------------------------
-- shapify: given a "Shape" and a flat array shapify gives, in 
-- some sence, an array where the elements have the desired shape 
-- and indexing is redirected into the flat array.
-- -----------------------------------------------------------------------------

-- DONE: fix write Offset addition 
-- TODO: Make sure this works.
shapify :: FData -> Arr DExp -> IndexE -> IndexE -> FData
shapify fd ws wOffset  = ixfFromFData fd ws wOffset

stride :: FData -> Int 
stride (Unit (a,t)) = 1
stride (Tuple xs)   = sum $ map stride xs
stride (For a)      = len a * (stride (a ! 0))

ixfFromFData :: FData -> Arr DExp -> IndexE -> IndexE -> FData
ixfFromFData fd ws wOffset = snd (process fd_stride fd ws wOffset 0)
   where 
     fd_stride = stride fd


process :: Int -> FData -> Arr DExp -> IndexE -> Int -> (Int, IndexE -> FData)
process fd_stride (Unit (a,t)) ws wOffset n = 
    (n+1 ,\ix ->  Unit (fixType t (ws ! (ix * (fromIntegral fd_stride)  + (fromIntegral  n) + wOffset)), t)) 
 

process fd_stride (Tuple [x,y]) ws wOffset n = 
    let 
        (n',ixf1)  = process fd_stride x ws wOffset n 
        (n'',ixf2) = process fd_stride y ws wOffset n' 
    in (n''+1, (\ix -> Tuple [ixf1 ix, ixf2 ix])) 
    




-- -----------------------------------------------------------------------------
-- fixType 
-- Must be made aware of "type casting" instructions as well.
-- -----------------------------------------------------------------------------
fixType :: Type -> DExp -> DExp 
fixType t (If e1 e2 e3)  = (If (fixType t e1) (fixType t e2) (fixType t e3))
fixType t (Op1 op e1)    = (Op1 op (fixType t e1))
fixType t (Op2 op e1 e2) = (Op2 op (fixType t e1) (fixType t e2))
fixType t (Index a b _) = Index a b t 
fixType t a = a 


-- -----------------------------------------------------------------------------
-- Add the "blockIdx" indexing 
-- -----------------------------------------------------------------------------
addOffsetDExp ::  [(Name,BlockSize)] -> DExp -> DExp 
addOffsetDExp offs a@(Index nom [] _) = a
--   error " WHAT TO DO IN THIS CASE ?"
addOffsetDExp offs (Index nom [ix] t) = 
    case lookup nom offs of 
      Nothing -> error "addOffsetDExp: Nothing!"
      Just i  -> Index nom [Op2 Add ix (unE (blockOffs i))] t
addOffsetDExp offs (Op2 op a b) = 
    Op2 op (addOffsetDExp offs a) (addOffsetDExp offs b)
addOffsetDExp offs (Op1 op a)   = 
    Op1 op (addOffsetDExp offs a) 
addOffsetDExp offs (If e1 e2 e3) = 
    If (addOffsetDExp offs e1) (addOffsetDExp offs e2) (addOffsetDExp offs e3)
addOffsetDExp offs a = a 


addOffset :: [(Name,BlockSize)] -> FData -> FData 
addOffset offs (Unit (e,t)) = Unit (addOffsetDExp offs e, t)
addOffset offs (Tuple xs) = Tuple $ fmap (addOffset offs) xs
addOffset offs (For arr)  = For $ fmap (addOffset offs) arr

blockOffs :: Int -> IndexE
blockOffs i = fromIntegral i * variable "blockIdx.x"

