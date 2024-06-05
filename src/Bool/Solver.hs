{-# LANGUAGE LambdaCase #-}
module Bool.Solver where 
import Bool.Raw 
import Bool.CNF 
import Bool.Parser 
import Bool.Translation
import Data.Map as M
import Control.Applicative(asum)

sat_tree_method :: CNF -> Maybe (Map String Bool) 
sat_tree_method (CNF clauses) = go clauses M.empty where 
  go :: [Clause] -> Map String Bool -> Maybe (Map String Bool)
  go [] m                      = Just m 
  go ((Clause lits):cs) litmap = let 
    branchmaps = go' lits litmap
    in asum $ fmap (go cs) branchmaps

  go' :: [Lit] -> Map String Bool -> [Map String Bool]
  go' [] _ = [] 
  go' (lit:lits) m = let 
    (name, desire) = case lit of Lit l -> (l, True); Neg l -> (l, False)
    in case m M.!? name of 
    Nothing     -> (M.insert name desire m) : go' lits m   
    Just status -> if status == desire 
      then m : go' lits m 
      else go' lits m -- kill this branch