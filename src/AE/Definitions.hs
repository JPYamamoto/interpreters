module AE.Definitions where

type Number = Double

-- Language Definition
data AE = Num {value :: Number}
        | Add {left :: AE, right:: AE}
        | Sub {left :: AE, right:: AE}

instance Show AE where
  show (Num n) = show n
  show (Add l r) = "{+ " ++ show l ++ " " ++ show r ++ "}"
  show (Sub l r) = "{- " ++ show l ++ " " ++ show r ++ "}"
