module Bool.CNF where 
import Data.List(intercalate)

data CNF where 
  CNF :: [Clause] -> CNF
instance Show CNF where 
  show (CNF clauses) = intercalate " && " $ map (\clause -> '(': show clause ++ ")") clauses 

data Clause where 
  Clause :: [Lit] -> Clause 

instance Show Clause where 
  show (Clause lits) = intercalate " || " $ map show lits
data Lit where 
  Lit :: String -> Lit 
  Neg :: String -> Lit

instance Show Lit where 
  show (Lit l) = l 
  show (Neg l) = '!':l