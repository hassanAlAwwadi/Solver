module Bool.Parser where 

import Bool.Raw
import Text.ParserCombinators.ReadP as P 
import Data.Char as C 

parser :: P.ReadP Raw
parser = P.chainl1 (P.skipSpaces *> lit <* P.skipSpaces) op where 
  lit = P.choice 
    [ (P.between (P.char '(') (P.char ')') parser)
    , (RLit <$> P.munch1 C.isAlpha)
    , (RNot <$  P.char '~' <*> parser) 
    , (RNot <$  P.char '!' <*> parser) 
    ]
  op = P.choice 
    [ ROr   <$ P.string "||"
    , RAnd  <$ P.string "&&" 
    , (\l r -> ROr (RNot l) r) <$ P.string "=>" 
    , RXor  <$ P.string "!=" 
    ] 

parse :: String -> Raw
parse s =  fst . last $ readP_to_S parser s 
