{-# LANGUAGE LambdaCase #-}
module Bool.Raw where 
  
data Raw where 
  RLit  :: String  -> Raw 
  RNot  :: Raw     -> Raw 
  RBin  :: Raw -> BoolBin -> Raw -> Raw 
    deriving Show

data BoolBin where 
  And  :: BoolBin
  Or   :: BoolBin
  Xor  :: BoolBin
  Nand :: BoolBin
  Nor  :: BoolBin
  Xnor :: BoolBin
  deriving Show


simplify :: Raw -> Raw 
simplify = \case 
  RLit  l  -> RLit l 
  RNot  l  -> case simplify l of 
    RLit _l       -> RNot (RLit _l) -- nothing to do 
    RNot _l       -> _l             -- double negation...
    RBin x And  y -> RBin x Nand y  -- negate the operator
    RBin x Or   y -> RBin x Nor  y 
    RBin x Xor  y -> RBin x Xnor y 
    RBin x Nand y -> RBin x And  y 
    RBin x Nor  y -> RBin x Or   y 
    RBin x Xnor y -> RBin x Xor  y 
  RBin x op y-> RBin  (simplify x) op (simplify y)

