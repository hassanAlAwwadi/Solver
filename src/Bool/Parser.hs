{-# LANGUAGE LambdaCase #-}
module Bool.Parser where 

import Bool.Raw
import Bool.CNF
import Text.ParserCombinators.ReadP as P 
import Data.Char as C 

raw_parser :: P.ReadP Raw
raw_parser = P.chainl1 (P.skipSpaces *> lit <* P.skipSpaces) op where 
  lit = P.choice 
    [ (P.between (P.char '(') (P.char ')') raw_parser)
    , (RLit <$> P.munch1 C.isAlpha)
    , (RNot <$  P.char '~' <*> raw_parser) 
    , (RNot <$  P.char '!' <*> raw_parser) 
    ]
  op = P.choice 
    [ flip RBin Or   <$ P.string "||"
    , flip RBin And  <$ P.string "&&" 
    , (\l r -> RBin (RNot l) Or r) <$ P.string "=>" 
    , flip RBin Xor  <$ P.string "!=" 
    ] 

raw_parse :: String -> Raw
raw_parse s =  fst . last $ readP_to_S raw_parser s 


{- 
 c Here is a comment.
 p cnf 5 3
 1 -5 4 0
 -1 5 3 4 0
 -3 -4 0
 -}
dimacs_cnf_parse :: String -> CNF 
dimacs_cnf_parse input = let 
  (comments_format_clauses) = lines input 
  (_, format_clauses)       = span (\xs -> case xs of 'c':_ -> True; _ -> False) comments_format_clauses
  (_, clauses_junk)         = splitAt 1 format_clauses
  (clauses, _)              = span (\xs -> case xs of '%':_ -> False; _ -> True) clauses_junk
  lits                      = map (init . words) clauses 
  parsed_lits               = (map . map) (\case ('-':lit) -> Neg lit; lit -> Lit lit) lits
  parsed_clauses            = map Clause parsed_lits
  in CNF parsed_clauses