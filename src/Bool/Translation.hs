{-# LANGUAGE LambdaCase #-}
module Bool.Translation where 
import Bool.Raw as R
import Bool.CNF as C 

tseytin :: Raw -> CNF
tseytin = form . go names [] . simplify where
  names = ('_' :) . show @Int <$> [1..] -- reserved names. "_0", "_1".. etc. Users are not allowed to use!.
  go :: [String] -> [[Lit]] -> Raw -> (String, [[Lit]], [String])
  go (n:ns) acc = \case 
    RLit  x   -> (x, acc, (n:ns))
    RNot  x   -> let 
      (x', acc', ns') = go ns acc x 
      in (n, [[Neg x', Neg n], [Lit x', Lit n]]  ++ acc'  ,ns')
    RAnd  x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Neg x', Neg y', Lit n],[Lit x', Neg n], [Lit y', Neg n]] ++ acc'', ns'')
    RNand x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Neg x', Neg y', Neg n],[Lit x', Lit n],[Lit y', Lit n]] ++ acc'', ns'')
    ROr   x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Lit x', Lit y', Neg n],[Neg x', Lit n],[Neg y', Lit n]] ++ acc'', ns'')
    RNor  x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Lit x', Lit y', Lit n],[Neg x', Neg n],[Neg y', Neg n]] ++ acc'', ns'')
    RXor  x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Neg x', Neg y', Neg n],[Lit x', Lit y', Neg n], [Lit x', Neg y', Lit n], [Neg x', Lit y', Lit n]] ++ acc'', ns'')
    RXnor  x y -> let 
      (x', acc',  ns' ) = go ns  acc  x
      (y', acc'', ns'') = go ns' acc' y 
      in (n, [[Neg x', Neg y', Lit n],[Lit x', Lit y', Lit n], [Lit x', Neg y', Neg n], [Neg x', Lit y', Neg n]] ++ acc'', ns'')

  form :: (String, [[Lit]], [String]) -> CNF 
  form (_, litss, _) = CNF $ fmap Clause ([Lit ("_0")] : litss)
