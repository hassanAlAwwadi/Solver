{-# LANGUAGE LambdaCase #-}
module Bool.Raw where 
  
data Raw where 
  RLit  :: String -> Raw 
  RNot  :: Raw    -> Raw 
  RAnd  :: Raw    -> Raw -> Raw 
  ROr   :: Raw    -> Raw -> Raw 
  RXor  :: Raw    -> Raw -> Raw
  -- | not parsed, there is a fold for them
  RNand :: Raw    -> Raw -> Raw
  RNor  :: Raw    -> Raw -> Raw
  RXnor :: Raw    -> Raw -> Raw
    deriving Show



simplify :: Raw -> Raw 
simplify = \case 
  RLit  l  -> RLit l 
  RNot  l  -> case simplify l of 
    RNot _l   -> _l 
    RAnd x y -> RNand x y 
    ROr  x y -> RNor x y 
    RXor x y -> RXnor x y 
    RNand x y -> RAnd x y 
    RNor  x y -> ROr x y 
    RXnor x y -> RXor x y 
    RLit _l    -> RNot (RLit _l)
  RAnd  x y-> RAnd  (simplify x) (simplify y)
  ROr   x y-> ROr   (simplify x) (simplify y)
  RXor  x y-> RXor  (simplify x) (simplify y)
  RNand x y-> RNand (simplify x) (simplify y)
  RNor  x y-> RNor  (simplify x) (simplify y)
  RXnor x y-> RXnor (simplify x) (simplify y)

