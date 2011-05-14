{-# LANGUAGE TypeOperators #-}

{- 
   Execute.hs, run kernels on the GPU or CPU
   Joel Svensson 
-}

module Obsidian.ArrowObsidian.Execute 
       (execute, 
        emulate) where 

import Obsidian.ArrowObsidian.Core
import Obsidian.ArrowObsidian.Exp 
import Obsidian.ArrowObsidian.Arr 
import Obsidian.ArrowObsidian.Flatten
import Obsidian.ArrowObsidian.Printing
import Obsidian.ArrowObsidian.CodeGen

import Obsidian.ArrowObsidian.PureAPI
import Obsidian.ArrowObsidian.API

import System.Directory
import System.FilePath
import System.Process
import System.IO

import Unsafe.Coerce
import Foreign.Marshal.Array
import Foreign.Ptr

import Data.List
import Data.Word

{-
  2009-10-26
    TODO: 
     Generalize castAllFData function (ETC) 
-}

{- PROBLEMS  
    
   In function Simple:
     FIXED PARTIALLY (2009-10-23: The shared memory requirements are 
         calculated incorrectly.
         Currently shared memory size is set to (2* olen * size (outputShape)))
         which is not correct. The actual requirement can be much higher 
         or lower. 
     The memory requirement is still sometimes calculated too high. 
     But it should not be the case that it is sometimes calculated too low!
 
   NEW STUFF 2009-10-19
    ¤ Slightly shortened the function createInputFile.
    ¤ Fixed bugs in function simple.
   
   TODO 2009-10-19
    ¤ Clean up and improve function simple. 
    ¤ look for more bugs + fix them. 
        ¤ Shared memory size  bug identified and partially fixed (2009-10-23) 
-} 
  
{- 2010-02-04  
 TODO: 
   generalise execute (should be possible using the new genKernel function).
   But probably takes a lot of work, I'm thinking mostly about 
   the dat file format.. should i keep doing it using files or 
   should i try to move to something different. 
-}
--------------------------------------------------------------------------------

data ExecMode = EMU | GPU

--------------------------------------------------------------------------------

execute :: (Flatten a, Flatten b) => 
             (Arr a :-> Arr b) -> [a] -> IO [b]  
execute = simple GPU

emulate :: (Flatten a, Flatten b) => 
             (Arr a :-> Arr b) -> [a] -> IO [b]  
emulate = simple EMU

--------------------------------------------------------------------------------

simple :: ( Flatten a, Flatten b) => 
          ExecMode -> (Arr a :-> Arr b) -> [a] -> IO [b]  
simple _ _ []   = error "empty input list" -- ULGY FIX
simple mode f list = 
    do 
      exec
    where 
      arr' = Arr (\ix -> (Index "input0" [unE ix] undefined)) 
                   nInputElements :: Arr DExp 
      arr  = readL inputShape arr'

      -- generate code that does not check bounds 
      kernel = genKernelMT "generated" f' arr res sm t 
      -- or code that does ? 
      -- kernel = genKernelMT "generated" f' arr res sm t True
               
      -- res    = runS f' arr  -- :: Arr FData
      ((sm,t), res) = runPrgMT f' arr

      inputShape = toFData (list !! 0)
      outputShape = (res ! 0)
      nInputElements = n * size (inputShape)

      olen   = len res
                     
      n      = length list 
      
      f'     = Pure (fmap (fromFData)) ->- f ->- Pure (fmap (toFData))
               
      exec = do 
        tmp_dir <- getTemporaryDirectory
        let fullpath = tmp_dir ++ pathSeparator : "GPU-HASKELL"
        createDirectoryIfMissing False fullpath
     
        writeFile (fullpath ++ (pathSeparator : name) ++ ".cu") 
                     (rest  kernel 
                            (nInputElements) 
                            (olen * size (outputShape))  
                            t (2*sm) ) -- 2*sm is an overestimate ! 
                    
        writeFile (fullpath ++ (pathSeparator : "Makefile")) 
                      (genMakefile name)
        working_dir <- getCurrentDirectory 
        setCurrentDirectory fullpath
        
        -- Create input dat file in current dir
        createInputFile list
                            
        -- run Make 
        pid1 <- case mode of 
                  GPU -> runCommand "make -s 2> messages.txt"
                  EMU -> runCommand "make emulation -s 2> messages.txt"
        waitForProcess pid1 -- make sure the executable is generated
        pid2 <- runCommand (fullpath ++ pathSeparator:name) 
        waitForProcess pid2 -- make sure output is generated 
        
        --  read the output dat file using outputShape
        result <- readOutputFile outputShape olen  
        setCurrentDirectory working_dir
        --removeDirectoryRecursive fullpath
        return $ (map fromFData result)
     
      name = "generated"
      readOutput file = 
          do 
            string  <- readFile file 
            let strings = words string 
            return (map read strings)

--------------------------------------------------------------------------------

rest str isize osize nthreads sm = 
    includes  ++ macroes ++ typedefs ++ str ++  
       cmain (
             "  word values[" ++ show isize ++ "];\n" ++ 
             "  word result[" ++ show osize ++ "];\n" ++
             "  word * dvalues;\n" ++
             "  word * dresult;\n" ++ 
             readDatFile isize ++ 
             "  cudaMalloc((void**)&dvalues, sizeof(word) * "++ show isize ++" ); \n" ++
             "  cudaMalloc((void**)&dresult, sizeof(word) * "++ show osize ++" ); \n" ++
             "  cudaMemcpy(dvalues, values, sizeof(word) * " ++ show isize ++", cudaMemcpyHostToDevice);\n" ++
             "  " ++ runKernel "generated" nthreads sm osize ++ 
             "  cudaMemcpy(result, dresult, sizeof(word) * "++ show osize ++" , cudaMemcpyDeviceToHost);\n" ++
             "  cudaFree(dvalues);\n" ++
             "  cudaFree(dresult);\n" ++
             writeDatFile (osize)
             )


--------------------------------------------------------------------------------
readDatFile :: Int -> String 
readDatFile n =  "  FILE *fp = NULL;\n" ++ 
                 "  fp = fopen(\"input.dat\",\"rb\");\n" ++
                 "  if (!fp)  {\n    fprintf(stderr,\"InputFile: Error\\n\");\n    exit(-1);\n  }\n" ++  
                 "  fread(values,sizeof(word),"++ show n ++ ",fp);\n"

writeDatFile :: Int -> String 
writeDatFile n = "  fp = fopen(\"output.dat\",\"wb\");\n" ++
                 "  if (!fp)  {\n    fprintf(stderr,\"OutputFile: Error\\n\");\n    exit(-1);\n  }\n" ++  
                 "  fwrite(result,sizeof(word),"++ show n ++ ",fp);\n"

createInputFile :: Flatten a => [a] -> IO () 
createInputFile inp = 
    
    do let dat = castAll inpFlat  {- castAllWord inpFlat -}
           numWords = length dat
       arr <- newArray dat
       withBinaryFile "input.dat" WriteMode 
             (\handle -> hPutBuf handle arr (numWords * 4) ) 
      
  
      where 
        inpFlat = map toFData inp 
        castAll = concatMap castFlatten

        castFlatten :: FData -> [Word32]
        castFlatten (Unit (LitInt i,Int)) = [unsafeCoerce i]
        castFlatten (Unit (LitFloat f,Float)) = [unsafeCoerce f]
        castFlatten (Unit (LitBool b,Bool)) = [unsafeCoerce b]
        castFlatten (Tuple more) = concatMap castFlatten more


readOutputFile :: FData -> Int -> IO [FData]
readOutputFile shape len = 
    
    do let wordsPerElem = size shape 
           bytesToRead  = (wordsPerElem * len * 4)
    --   case shape of
    --     Unit _ -> putStrLn $ "Single element. size: " ++ show (size shape)
    --     Tuple _ -> putStrLn $ "Pair of something. size: " ++ show (size shape)
                          
       arr <- mallocArray (wordsPerElem * len) :: IO (Ptr Word32)
       withBinaryFile "output.dat" ReadMode 
             (\handle -> 
                  do r <- hGetBuf handle arr bytesToRead
                     case (r < bytesToRead) of 
                       True -> error "readOutputFile: Error output file too small"
                       False -> return ()
                     list <- peekArray  (wordsPerElem * len) arr
                     -- putStrLn (show (length list))
                     return (castAllFData shape list))
    where
      castAllFData :: FData -> [Word32] -> [FData]
      castAllFData _ [] = [] 
      castAllFData s@(Unit (_,Int)) (x:xs) 
          = (Unit (LitInt (unsafeCoerce x),Int)) : castAllFData s xs
      castAllFData s@(Unit (_,Float)) (x:xs) 
          = (Unit (LitFloat (unsafeCoerce x),Float)) : castAllFData s xs
      castAllFData s@(Tuple [p1,p2]) xs 
          = (Tuple (concat [castAllFData p1 first,
                            castAllFData p2 second])) : castAllFData s rest
            where s1 = size p1
                  s2 = size p2 
                  rest  = drop (s1+s2) xs
                  first = take s1 xs 
                  second = take s2 (drop s1 xs) 
      castAllFData s@(Tuple [p1,p2,p3]) xs 
          = (Tuple (concat [castAllFData p1 first,
                            castAllFData p2 second,
                            castAllFData p3 third])) : castAllFData s rest
            where s1 = size p1
                  s2 = size p2 
                  s3 = size p3
                  rest  = drop (s1+s2+s3) xs
                  first = take s1 xs 
                  second = take s2 (drop s1 xs) 
                  third  = take s3 (drop (s1+s2) xs)
      castAllFData s@(For _) _ = error $ "castAllFData: For not supported"  
      castAllFData s _ = error $ "castAllFData: " ++ show s 

 
       
--------------------------------------------------------------------------------

outputData (Unit a) olen = 
    "  for(int i = 0; i < " ++ show olen ++ "; i++){\n" ++ 
    "    printf(\"%d \",result[i]);\n" ++
    "  }\n" 

outputData (Tuple [a,b]) olen = 
    "  for(int i = 0; i < " ++ show (olen `div` 2)++ "; i++){\n" ++ 
    "    printf(\"(%d,\",result[i*2]);\n" ++
    "    printf(\"%d) \",result[i*2+1]);\n" ++
    "  }\n" 


-- -----------------------------------------------------------------------------       
includes = "#include <stdio.h>\n" ++ 
           "#include <stdlib.h>\n"

typedefs = "typedef unsigned int word;\n"

macroes =  "#define ix_double(arr,i) ((double*)(arr+(i)))[0]\n" ++
           "#define ix_float(arr,i)  ((float*)(arr+i))[0]\n" ++ 
           "#define ix_int(arr,i)    ((int*)(arr+i))[0]\n" 
           
           
cmain str = "int main(int argc, char **argv){\n" ++ str ++ "\n}\n"
       

-- -----------------------------------------------------------------------------
runKernel :: String -> Int -> Int -> Int -> String 
runKernel name threads sm olen = name ++ "<<<1, " ++ show threads ++ 
                          "," ++ show sm ++ "* sizeof(unsigned int)>>>(dvalues,dresult," ++ show olen ++ ");\n"


strListToCArrayStr :: [String] -> String 
strListToCArrayStr xs = '{':((concat $ Data.List.intersperse "," xs) ++ "}")

listToCArrayStr :: Show a => [a] -> String
listToCArrayStr xs = '{':((concat $ Data.List.intersperse "," (map show xs)) ++ "}")


--------------------------------------------------------------------------------


