-- This requires GHC 7.4 or above
-- Invoke with "ghci -package ghc Fhwc.hs"
-- then something like ":main examples/AddPrim.hs"

module Main where

-- http://lambda.haskell.org/platform/doc/current/ghc-api/DynFlags.html
import GHC ( CoreModule, defaultErrorHandler, runGhc, getSessionDynFlags, 
             setSessionDynFlags, compileToCoreSimplified )
import DynFlags ( defaultLogAction, updOptLevel )
import Outputable ( showPpr )
import GHC.Paths ( libdir ) -- GHC.Paths from "cabal install ghc-paths"

import System.Environment ( getArgs )
import Control.Monad ( when )
import System.Exit ( exitFailure )
import System.Console.GetOpt

import CoreToIR
import CombinationPass ( combination_pass )
import RemoveRecursionPass ( tail_recursion_pass, module_recursiveness_stats )

import IRInterpreter ( runMain )
import IRToHaskell ( moduleToHaskell, showLiteral )
import IRToOCaml ( moduleToOCaml )

main :: IO ()
main = do
  args <- getArgs
  (sourceFilename, flags) <- getFlags args
  let ifFlag f m = when (f `elem` flags) m
  
  coreModule <- compileToCore sourceFilename
  let (ir, topLevelVars) = coreModuleToIR coreModule
  let combinedIR = combination_pass ir topLevelVars
  let transform = if NoTransform `notElem` flags 
                  then tail_recursion_pass
                  else id
  let tailIR = transform combinedIR

  ifFlag ShowCore (putStrLn $ showPpr coreModule)
  ifFlag ShowIR (putStrLn $ moduleToHaskell ir)
  ifFlag ShowHaskell (putStrLn $ moduleToHaskell tailIR)
  ifFlag ShowOCaml (putStrLn $ moduleToOCaml tailIR) 
  
  -- for debugging only
  ifFlag ShowMain (putStrLn $ showLiteral (runMain tailIR))
  ifFlag ShowStats (putStrLn $ module_recursiveness_stats tailIR)


-----------------------------------------------------

data Flag = ShowIR
          | ShowCore
          | ShowHaskell
          | ShowOCaml
          | ShowMain
          | ShowStats
          | NoTransform
          deriving (Eq)

options :: [OptDescr Flag]
options =
 [ Option ['c'] [] (NoArg ShowCore)         "show core"
 , Option ['i'] [] (NoArg ShowIR)           "show ir"
 , Option ['n'] [] (NoArg NoTransform)      "don't make ir tail-recursive-only"
 , Option ['h'] [] (NoArg ShowHaskell)      "generate Haskell"
 , Option ['o'] [] (NoArg ShowOCaml)        "generate OCaml"
 , Option ['m'] [] (NoArg ShowMain)         "show main"
 , Option ['s'] [] (NoArg ShowStats)        "show recursiveness stats"
 ]

getFlags :: [String] -> IO (String, [Flag])
getFlags args = 
    case getOpt Permute options args of
       ([], _, []) -> cmdlnFail Nothing
       (flags, [file], []) -> return (file, flags)
       (_, _, errs) -> cmdlnFail $ Just (concat errs) 
    where
    cmdlnFail :: Maybe String -> IO a
    cmdlnFail msg = do 
        case msg of
            Just s -> putStr $ "error: " ++ s
            _ -> return ()
        putStr $ usageInfo header options
        exitFailure
        where header = "Usage: fhwc [options] file"

-----------------------------------------------------
-- See http://www.haskell.org/haskellwiki/GHC/As_a_library

compileToCore :: FilePath -> IO CoreModule
compileToCore filename = 
    defaultErrorHandler defaultLogAction $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = updOptLevel 1 dflags
        _ <- setSessionDynFlags dflags'
        compileToCoreSimplified filename

