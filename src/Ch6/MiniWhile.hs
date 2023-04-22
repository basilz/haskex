module Ch6.MiniWhile(main, parseEval, program) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State
import Data.Functor (($>))
import qualified Data.Functor.Identity
import qualified Data.Map as M
import qualified Text.Parsec.Token as Token
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Test.Hspec

newtype Program = Program [Stmt] deriving (Show, Eq)

data Stmt
  = Asgn Id Exp
  | While Exp [Stmt]
  deriving (Show, Eq)

data CmpOp
  = TLE
  | TGT
  | TEQ
  | TNEQ
  deriving (Show, Eq)

data Exp
  = If Exp Exp Exp
  | Cmp CmpOp Aexp Aexp
  | Not Exp
  | Aexp Aexp
  deriving (Show, Eq)

data Op
  = TAdd
  | TMult
  | TDiv
  | TSub
  deriving (Show, Eq)

data Aexp
  = Num Integer
  | Var Id
  | Brk Op Aexp Aexp
  deriving (Show, Eq)

type Id = String

languageDef :: Token.LanguageDef u
languageDef =
  emptyDef
    { Token.commentStart = "/*",
      Token.commentEnd = "*/",
      Token.commentLine = "//",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "if",
          "then",
          "else",
          "while",
          "do",
          "true",
          "false",
          "not",
          "and",
          "or"
        ],
      Token.reservedOpNames =
        [ "+",
          "-",
          "*",
          "/",
          ":=",
          "==",
          "<=",
          ">",
          "and",
          "or",
          "not"
        ]
    }

lexer :: Token.GenTokenParser String u Data.Functor.Identity.Identity
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer -- parses an identifier

reserved :: String -> Parser ()
reserved = Token.reserved lexer -- parses a reserved name

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer -- parses an operator

parens :: Parser a -> Parser a
parens = Token.parens lexer -- parses surrounding parenthesis:

integer :: Parser Integer
integer = Token.integer lexer -- parses an integer

semi :: Parser String
semi = Token.semi lexer -- parses a semicolon

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer -- parses whitespace

asgn :: Parser Stmt
asgn = do
  var <- identifier
  reservedOp ":="
  Asgn var <$> pexp

while :: Parser Stmt
while = do
  reserved "while"
  cond <- pexp
  reserved "do"
  stmts <- stmt `sepBy1` semi
  reserved "done"
  return $ While cond stmts

ifStmt :: Parser Exp
ifStmt = do
  reserved "if"
  exp1 <- pexp
  reserved "then"
  exp2 <- pexp
  reserved "else"
  If exp1 exp2 <$> pexp

aexp :: Parser Aexp
aexp = aTerm `chainl1` aMult `chainl1` aAdd

aAdd :: Parser (Aexp -> Aexp -> Aexp)
aAdd = reservedOp "+" $> Brk TAdd <|> reservedOp "-" $> Brk TSub

aMult :: Parser (Aexp -> Aexp -> Aexp)
aMult = reservedOp "*" $> Brk TMult <|> reservedOp "/" $> Brk TDiv

aTerm :: Parser Aexp
aTerm =
  parens aexp
    <|> Var <$> identifier
    <|> Num <$> integer

pexp :: Parser Exp
pexp = ifStmt <|> try pCmp <|> notExp <|> Aexp <$> aexp

notExp :: Parser Exp
notExp = reserved "not" >> Not <$> pexp

pCmp :: Parser Exp
pCmp = do
  aexp1 <- aexp
  cmp <-
    (reservedOp "<=" >> return TLE)
      <|> (reservedOp "==" >> return TEQ)
      <|> (reservedOp "!=" >> return TNEQ)
      <|> (reservedOp ">" >> return TGT)
  Cmp cmp aexp1 <$> aexp

stmt :: Parser Stmt
stmt = while <|> asgn

program :: Parser Program
program = do
  whiteSpace
  lStmt <- stmt `sepBy1` semi
  return $ Program lStmt

type Env = M.Map Id Integer

data EvalError = 
  ParseError ParseError
  | VariableNotDefined 
  | IntegerInBoolean 
  | BooleanInInteger deriving (Eq, Show)

type Evaluator = ExceptT EvalError (StateT Env IO)

evalStmt :: Stmt -> Evaluator ()
evalStmt (Asgn i e) = do
  x <- evalExp e
  modify $ M.insert i x
evalStmt w@(While prop stmts) = do
  mapM_ evalStmt stmts
  propTrue <- evalProp prop
  when propTrue $ evalStmt w

evalProp :: Exp -> Evaluator Bool
evalProp (Cmp op e1 e2) = do
  x <- evalAexp e1
  y <- evalAexp e2
  case op of
    TLE -> return $ x <= y
    TGT -> return $ x > y
    TEQ -> return $ x == y
    TNEQ -> return $ x /= y
evalProp (Not prop) = do
  p <- evalProp prop
  return $ not p
evalProp _ = throwError IntegerInBoolean

evalExp :: Exp -> Evaluator Integer
evalExp (If prop e1 e2) = do
  propTrue <- evalProp prop
  if propTrue then evalExp e1 else evalExp e2
evalExp (Aexp e) = evalAexp e
evalExp _ = throwError BooleanInInteger

evalAexp :: Aexp -> Evaluator Integer
evalAexp (Num x) = return x
evalAexp (Var v) = do
  env <- get
  let x = M.lookup v env
  case x of
    Nothing -> throwError VariableNotDefined
    Just n -> return n
evalAexp (Brk op ae1 ae2) = do
  x <- evalAexp ae1
  y <- evalAexp ae2
  case op of
    TAdd -> return $ x + y
    TMult -> return $ x * y
    TDiv -> return $ x `div` y
    TSub -> return $ x - y

eval :: Program -> Evaluator Env
eval p@(Program stmts) = do
  liftIO $ print p
  mapM_ evalStmt stmts
  get

parseEval :: String -> IO (Either EvalError Env)
parseEval s = 
  let x = parse program "" s
  in case x of
    Left parseError -> return $ Left (ParseError parseError)
    Right prg -> do
      (e, _) <- runStateT (runExceptT (eval prg)) M.empty
      case e of
        err@(Left _) -> return err
        Right env -> return $ Right env

-- main :: IO ()
-- main = do 
--   x <- parseEval "x:= 0; y:= 5;while x <= 3 do y:= (y * 5); x:= (x + 1) done; y:= if y > 10000 then 10000 else y fi"
--   case x of
--     Left ee -> print ee
--     Right env -> print env

main :: IO ()
main = hspec $ do 
  describe "x := 3" $ do
    it "should be evaluated as" $ do
      parsed <- parseEval "x := 3"
      parsed `shouldBe` Right (M.singleton "x" 3)
