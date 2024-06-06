module Main (main) where
import Bool.Parser
import Bool.Solver
import System.IO
import System.Directory
import Data.Foldable(for_)
main :: IO ()
main = do 
  satisfiable_files   <- take 20 <$> listDirectory "C:/CS/Solver/exprs/sat"
  withCurrentDirectory "C:/CS/Solver/exprs/sat" $ do 
    for_ satisfiable_files $ \file -> withFile file ReadMode $ \handle -> do
      raw_expr <- hGetContents handle
      let cnf_expr = dimacs_cnf_parse raw_expr
      let sat      = sat_filter cnf_expr 
      case sat of 
        Nothing -> putStrLn ("FAIL - File: " ++ file ++ "produced UNSAT but is SAT" )
        Just _  -> putStrLn ("SUCC - File: " ++ file ++ "produced   SAT and is SAT" )
  unsatisfiable_files <- take 20 <$> listDirectory "C:/CS/Solver/exprs/unsat"
  print unsatisfiable_files
  withCurrentDirectory "C:/CS/Solver/exprs/unsat" $ do 
    for_ unsatisfiable_files $ \file -> withFile file ReadMode $ \handle -> do
      raw_expr <- hGetContents handle
      let cnf_expr = dimacs_cnf_parse raw_expr
      let sat      = sat_filter cnf_expr 
      case sat of 
        Just _  -> {-setSGR [SetColor Foreground Vivid Red]   *>-} putStrLn ("FAIL - File: " ++ file ++ "produced   SAT but is UNSAT") 
        Nothing -> {-setSGR [SetColor Foreground Vivid Green] *>-} putStrLn ("SUCC - File: " ++ file ++ "produced UNSAT and is UNSAT")
