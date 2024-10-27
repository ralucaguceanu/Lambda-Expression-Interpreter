module Binding where

import qualified Data.Map as Map

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx strategy expr = do
    result <- applyMacros ctx expr
    return $ simplify strategy result

applyMacros :: Context -> Lambda -> Either String Lambda
applyMacros ctx expr = case expr of
    Var x -> Right $ Var x
    Abs x e -> Right $ Abs x e

    App e1 e2 -> do
        simplifiedE1 <- applyMacros ctx e1
        simplifiedE2 <- applyMacros ctx e2
        return $ App simplifiedE1 simplifiedE2

    Macro m -> case lookup m ctx of
        Just replacement -> Right replacement
        Nothing -> Left $ "Macro '" ++ m ++ "' not found"

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
