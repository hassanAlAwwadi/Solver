{-# LANGUAGE LambdaCase #-}
module Bool.Solver where 
import Bool.CNF 
import qualified Data.Map as M
import qualified Data.List as L
import Data.Map(Map)
import Control.Applicative(asum)

sat_brute_force :: CNF -> Maybe (Map String Bool) 
sat_brute_force (CNF clauses) = go clauses M.empty where 
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

sat_filter :: CNF -> Maybe (Map String Bool)
sat_filter (CNF clauses) = go (L.sortOn length $ map (\(Clause c) -> c) clauses) M.empty where
  go :: [[Lit]] -> Map String Bool -> Maybe (Map String Bool)
  go [] acc = Just acc
  go (l:ls) acc = asum $ flip fmap l $ \lit -> let 
  -- | variable name, desired value, and opposite form
    (n, d, o) = case lit of Lit v -> (v, True, Neg v); Neg v -> (v, False, Lit v)
    acc'    = M.insert n d acc  
  -- | If we have set a to True, we can filter all clauses with a in them,
  -- | and if we've set it to false, we can filter all clauses with !a in them.
    ls'  = filter (notElem lit) ls      
  -- | If we've set a to True, we can remove !a from all clauses, since we can't set it to false anymore (and vice versa). 
  -- | This works out because if we remove all literals from a clause, asum [] will return Nothing :)
    ls'' = map    (filter (/= o)) ls'
  -- | We reorder based on length of the lists, to narrow down branching algorithms.
  -- | Since we know the maxsize is 3, and the minsize is 0, there is probably a better bounded sorting algorithm
    ls''' = L.sortOn length ls''
    in go ls''' acc'                     
    