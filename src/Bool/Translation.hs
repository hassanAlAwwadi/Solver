{-# LANGUAGE LambdaCase #-}
module Bool.Translation where 
import Bool.Raw as R
import Bool.CNF as C 

tseytin :: Raw -> CNF
tseytin = form . go names [] . simplify where
  names = ('_' :) . show @Int <$> [0..] -- reserved names. "_0", "_1".. etc. Users are not allowed to use!.
  go :: [String] -> [[Lit]] -> Raw -> (String, [[Lit]], [String])
  go (n:ns) acc = \case 
    RLit  x   -> (x, acc, (n:ns))
    RNot  x   -> let 
      (x', acc', ns') = go ns acc x 
      in (n, [[Neg x', Neg n], [Lit x', Lit n]]  ++ acc'  ,ns')
    RBin x op y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      conj              = handle_op n x' y' op 
      in (n, conj ++ acc'', ns'')

  handle_op :: String -> String -> String -> BoolBin ->  [[Lit]] 
  handle_op n x y = \case 
    And  -> [[Neg x, Neg y, Lit n], [Lit x, Neg n], [Lit y, Neg n]]
    Nand -> [[Neg x, Neg y, Neg n], [Lit x, Lit n], [Lit y, Lit n]]
    Or   -> [[Lit x, Lit y, Neg n], [Neg x, Lit n], [Neg y, Lit n]]
    Nor  -> [[Lit x, Lit y, Lit n], [Neg x, Neg n], [Neg y, Neg n]]
    Xor  -> [[Neg x, Neg y, Neg n], [Lit x, Lit y, Neg n], [Lit x, Neg y, Lit n], [Neg x, Lit y, Lit n]]
    Xnor -> [[Neg x, Neg y, Lit n], [Lit x, Lit y, Lit n], [Lit x, Neg y, Neg n], [Neg x, Lit y, Neg n]]

  form :: (String, [[Lit]], b) -> CNF 
  form (name, litss, _) = CNF $ fmap Clause ([Lit name] : litss)
