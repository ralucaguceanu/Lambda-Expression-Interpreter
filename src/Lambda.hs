module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = vars e1 ++ vars e2
vars (Abs x e) = x : filter (\v -> v /= x) (vars e)
vars (Macro _) = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = filter (\v -> v /= x) (freeVars e)
freeVars (Macro _) = []

-- 1.3.
newVar :: [String] -> String
newVar list = head $ filter (\c -> notElem c list) allStrings
    where
        allStrings = alphabet ++ alphabet_2 ++ alphabet_3
        alphabet = map (:[]) ['a'..'z']
        alphabet_2 = [a ++ b | a <- alphabet, b <- alphabet]
        alphabet_3 = [a ++ b ++ c | a <- alphabet, b <- alphabet, c <- alphabet]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var x) = True
isNormalForm (App (Abs x e) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs x e) = isNormalForm e
isNormalForm (Macro _) = True

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var y) e = 
    if x == y then e else Var y
reduce x (App e1 e2) e = App (reduce x e1 e) (reduce x e2 e)
reduce x (Abs y e1) e2
    | x == y = Abs y e1
    | y `elem` freeVars e2 = Abs y' (reduce x e1' e2)
    | otherwise = Abs y (reduce x e1 e2)
        where
            y' = newVar (y : x : freeVars e2 ++ freeVars e1)
            e1' = reduce y e1 (Var y')
reduce _ (Macro m) _ = Macro m

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e) e2) = reduce x e e2
normalStep (App e1 e2) = if isNormalForm e1 then App e1 (normalStep e2) else App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e) e2) =
    if isNormalForm e2
        then reduce x e e2
        else App (Abs x e) (applicativeStep e2)
applicativeStep (App e1 e2) =
    if isNormalForm e1
        then App e1 (applicativeStep e2)
        else App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify stepFunc e =
    if isNormalForm e
        then [e]
        else e : simplify stepFunc (stepFunc e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
