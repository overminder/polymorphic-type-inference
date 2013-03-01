module Parser (
  readProgram
) where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as T

import Core

readProgram :: String -> [Binding ()]
readProgram str = case parse pProgram "<Core program>" str of
  Left e -> error $ show e
  Right r -> r

-- Syntax defs
pProgram = ws >> many pSupercomb

languageDef
  = emptyDef { T.commentStart    = "{-"
             , T.commentEnd      = "-}"
             , T.nestedComments  = True
             , T.commentLine     = "--"
             , T.identStart      = letter <|> char '_'
             , T.identLetter     = alphaNum <|> char '_'
             , T.reservedNames   = [ "if"
                                   , "then"
                                   , "else"
                                   , "let"
                                   , "letrec"
                                   , "in"
                                   ]
             , T.reservedOpNames = words ("+ - * / = < <= > >= " ++
                                          "== != && || ! % ~ & | ^ " ++
                                          "<< >> :")
             , T.caseSensitive   = True
             }

lexer = T.makeTokenParser languageDef

-- Token defs
ident = T.identifier lexer
reserved = T.reserved lexer
reservedOp = T.reservedOp lexer
parens = T.parens lexer
braces = T.braces lexer
brackets = T.brackets lexer
natLit = T.natural lexer
numLit = T.naturalOrFloat lexer
strLit = T.stringLiteral lexer
chrLit = T.charLiteral lexer
semi = T.semi lexer
comma = T.comma lexer
ws = T.whiteSpace lexer

pSupercomb = pBinding

pExpr = pIfExpr <|> pLetrecExpr <|> pLetExpr <|> pLamExpr <|> pInfixExpr

pIfExpr = do
  reserved "if"
  e1 <- pExpr
  reserved "then"
  e2 <- pExpr
  reserved "else"
  e3 <- pExpr
  return $ foldl1 (EAp ()) [EVar () "if#", e1, e2, e3]

pLetExpr = do
  reserved "let"
  bindings <- many pBinding
  reserved "in"
  body <- pExpr
  return $ ELet () False bindings body

pLetrecExpr = do
  reserved "letrec"
  bindings <- many pBinding
  reserved "in"
  body <- pExpr
  return $ ELet () True bindings body

pBinding = do
  name : args <- many1 ident
  reservedOp "="
  rhs <- pExpr
  semi
  return (Binding () name args rhs)

pLamExpr = do
  reservedOp "\\"
  args <- many1 ident
  reservedOp "->"
  rhs <- pExpr
  return $ ELam () args rhs

pInfixExpr = buildExpressionParser opList pTerm

opList = [ [ Infix (reservedOp "*"  >> return (mkBinOp "(*)")) AssocLeft
           , Infix (reservedOp "/"  >> return (mkBinOp "(/)")) AssocLeft
           ]
         , [ Infix (reservedOp "+"  >> return (mkBinOp "(+)")) AssocLeft
           , Infix (reservedOp "-"  >> return (mkBinOp "(-)")) AssocLeft
           ]
         , [ Infix (reservedOp "<"  >> return (mkBinOp "(<)")) AssocLeft
           ]
         ]

mkBinOp ratorName lhs rhs = foldl1 (EAp ()) [EVar () ratorName, lhs, rhs]

pTerm = do
  func <- pAtom
  args <- many (try pAtom)
  return $ foldl (EAp ()) func args

pAtom = try pTuple2
    <|> parens pExpr
    <|> pList
    <|> liftM (EVar ()) ident
    <|> pNum

pTuple2 = parens $ do
  lhs <- pExpr
  comma
  rhs <- pExpr
  return $ foldl1 (EAp ()) [EVar () "(,)", lhs, rhs]

pList = brackets $ do
  xs <- pExpr `sepBy` comma
  return $ foldr (\x y -> foldl1 (EAp ()) [EVar () "(:)", x, y])
                 (EVar () "[]") xs

pNum = do
  num <- numLit
  return $ case num of
    Left i -> ELit () (LInt (fromIntegral i))
    Right d -> ELit () (LFloat d)

