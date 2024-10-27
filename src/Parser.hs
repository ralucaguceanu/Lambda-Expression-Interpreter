module Parser (parseLambda, parseLine) where

import Data.Char(isAlpha, isAlphaNum, isUpper, isDigit)

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.Type.Bool (Not)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- 2.1. / 3.2.
failParser :: Parser a 
failParser = Parser $ \s -> Nothing

charParser :: Char -> Parser Char
charParser c = Parser $
    \s -> case s of
        [] -> Nothing
        (x:xs) -> if x == c then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $
    \s -> case s of
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing

instance Monad Parser where
    mp >>= f = Parser $ \s -> 
        case parse mp s of
            Nothing -> Nothing
            Just (val, rest) -> parse (f val) rest 
    return x = Parser $ \s -> Just (x,s)

instance Applicative Parser where
    af <*> mp =
        do
            f <- af
            v <- mp
            return $ f v
    pure = return


instance Functor Parser where
    fmap f mp =
        do
            x <- mp
            return $ f x

instance Alternative Parser where
    empty = failParser
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                    Nothing -> parse p2 s
                                    x -> x

plusParser :: (Parser a) -> Parser [a]
plusParser p = do
        x <- p
        xs <- starParser p
        return $ x:xs

starParser :: (Parser a) -> Parser [a]
starParser p = (plusParser p) <|> (return [])

varParser :: Parser String
varParser = do
        x <- predicateParser isAlpha
        xs <- starParser (predicateParser isAlphaNum)
        return (x:xs)

macroParser :: Parser String
macroParser = do
        x <- predicateParser (\c -> isUpper c || isDigit c)
        xs <- starParser (predicateParser isAlphaNum)
        return (x:xs)

varExprParser :: Parser Lambda
varExprParser = do
            s <- varParser
            return (Var s)

macroExprParser :: Parser Lambda
macroExprParser = do
            s <- macroParser
            return (Macro s)

parseAbs :: Parser Lambda
parseAbs = do
    _ <- charParser '\\'
    x <- varParser
    _ <- charParser '.'
    e <- parseExpr
    return (Abs x e)

parseApp :: Parser Lambda
parseApp = do
    _ <- charParser '('
    e1 <- parseExpr
    _ <- charParser ' '
    e2 <- parseExpr
    _ <- charParser ')'
    return (App e1 e2)

parseExpr :: Parser Lambda
parseExpr = macroExprParser <|> varExprParser <|> parseAbs <|> parseApp

parseLambda :: String -> Lambda
parseLambda str = case parse parseExpr str of
    Just (result, "") -> result
    _ -> error "Parsing failed"

-- 3.3.
parseEval :: Parser Line
parseEval = Eval <$> parseExpr

parseBinding :: Parser Line
parseBinding = do
    name <- macroParser
    _ <- charParser '='
    expr <- parseExpr
    return (Binding name expr)

parseLine :: String -> Either String Line
parseLine str = case parse parseBinding str of
    Just (result, "") -> Right result
    _ -> case parse parseEval str of
        Just (result, "") -> Right result
        _ -> Left "Parsing failed"
