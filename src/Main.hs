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
  Abs t -> Abs(subst (j+1) (shift 1 0 s) t)
  App t1 t2 -> App (subst j s t1) (subst j s t2)

substTop s t = shift (-1) 0 (subst 0 (shift 1 0 s) t)

isval :: Term -> Bool
isval (Abs _) = True
isval _ = False

eval :: Term -> Term
eval (App t v) | isval v = substTop v t
               | isval t = substTop t v
               | otherwise = undefined
                      
main = do
  let x = Arg 0
      y = Abs(Arg(1))
      z = App x y
  putStrLn . show $ z
  putStrLn . show $ subst 0 (App (Arg 1) (Abs (Arg 2 ))) z
