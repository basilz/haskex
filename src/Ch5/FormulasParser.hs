module Ch5.FormulasParser where

import Text.Parsec
    ( char, digit, between, chainl1, many1, (<|>), skipMany )
import Text.Parsec.Text (Parser)
import Text.Parsec.Char (space)
import Data.Functor (($>))

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a) deriving Show

eval :: Num a => Expr a -> a
eval (Lit e) = e
eval (Add e1 e2) = eval e1 + eval e2
eval (Mult e1 e2) = eval e1 * eval e2

spaces :: Parser ()
spaces = skipMany space

symbol :: Char -> Parser Char
symbol s = char s <* spaces

parseExpr :: Parser (Expr Integer)
parseExpr = parseSubExpr `chainl1` parseMult `chainl1` parseAdd

-- parseExpr0 :: Parser (Expr Integer)
-- parseExpr0 = parseSubExpr `chainl1` parseMult

-- parseExpr1 :: Parser (Expr Integer)
-- parseExpr1 = parseExpr0 `chainl1` parseAdd

parseSubExpr :: Parser (Expr Integer)
parseSubExpr = between (symbol '(') (symbol ')') parseExpr <|> parseLit

parseLit :: Parser (Expr Integer)
parseLit = Lit . read <$> (spaces *> many1 digit <* spaces)

parseAdd :: Parser (Expr a -> Expr a -> Expr a)
parseAdd = symbol '+' $> Add

parseMult :: Parser (Expr a -> Expr a -> Expr a)
parseMult = symbol '*' $> Mult


