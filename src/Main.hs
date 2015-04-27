module Main where

data Term = Arg Integer
            | Abs Term
            | App Term Term

instance Show Term where
  show (Arg n) = show n
  show (Abs t) = "λ. " ++ show t
  show (App t1 t2) = show t1 ++ " " ++ show t2

shift :: Integer -> Integer -> Term -> Term
shift d c t = case t of
  arg@(Arg n) -> if n >= c then Arg (n + d) else arg
  Abs t -> Abs (shift d (c+1) t)
  App t1 t2 -> App (shift d c t1) (shift d c t2)

subst :: Integer -> Term -> Term -> Term
subst j s t = case t of
  arg@(Arg n) -> if n==j then s else arg
  Abs t -> subst (j+1) (shift 1 0 s) t
  App t1 t2 -> App (subst j s t1) (subst j s t2)
  
main = do
  let x = (Arg 1)
      y = subst 1 (Arg 3) x
  putStrLn . show $ y
  putStrLn . show $ x
