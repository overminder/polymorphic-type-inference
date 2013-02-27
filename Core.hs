{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Core (
  Expr(..), Name, IsRec,
  Binding(..),
  Literal(..),
  HasAnnotation(..),

  InlineTypePpr(..),
  pprExpr, pprBinding,
) where

import Util

import Data.Traversable
import Data.Foldable

type Name = String
type IsRec = Bool

class HasAnnotation t where
  annotationOf :: t a -> a

data Binding a
  = Binding {
    bind_anno :: a,
    bind_name :: Name,
    bind_args :: [Name],
    bind_body :: Expr a
  }
  deriving (Show, Functor, Foldable, Traversable)

instance HasAnnotation Binding where
  annotationOf = bind_anno

-- a: the annotation
data Expr a
  = EVar a Name
  | ELet a IsRec [Binding a] (Expr a)
  | ELit a Literal
  | ELam a [Name] (Expr a)
  | EAp a (Expr a) (Expr a)
  deriving (Show, Functor, Foldable, Traversable)

data Literal
  = LString String
  | LInt Int
  | LFloat Double
  | LChar Char
  deriving (Show)

instance HasAnnotation Expr where
  annotationOf e = case e of
    EVar a _ -> a
    ELet a _ _ _ -> a
    ELit a _ -> a
    ELam a _ _ -> a
    EAp a _ _ -> a

class InlineTypePpr a where
  pprAsBinding :: a -> Doc -> Doc
  pprAsVar :: a -> Doc -> Doc

instance InlineTypePpr () where
  pprAsBinding _ _ = empty
  pprAsVar _ doc = doc

instance InlineTypePpr a => Ppr (Expr a) where
  ppr = pprExpr

pprExpr :: InlineTypePpr a => Expr a -> Doc
pprExpr e = case e of
  EVar a name -> pprAsVar a (text name)
  ELet a isRec bindings body ->
    text ("let" ++ (if isRec then "rec" else "")) $$ nest 2
      (vcat (map pprBinding bindings)) $$
      text "in" <+> pprExpr body
  ELit a lit -> pprAsVar a (pprLiteral lit)
  ELam a args body -> pprAsVar a (text "\\" <> hsep (map text args) <+>
                                  text "->" <+> pprExpr body)
  EAp a _ _ -> pprAsVar a (pprEAp False e)

  where
    pprEAp needParen e = case e of
      EAp _ _ _ -> maybeParen (hsep (map (pprEAp True) (toList e)))
      _ -> pprExpr e
      where
        toList e = case e of
          EAp _ e1 e2 -> toList e1 ++ [e2]
          _ -> [e]

        maybeParen = if needParen then parens else id

instance InlineTypePpr a => Ppr (Binding a) where
  ppr = pprBinding

pprBinding :: InlineTypePpr a => Binding a -> Doc
pprBinding (Binding anno name args body) =
  pprAsBinding anno (text name) $$
  text name <+> hsep (map text args) <+> equals <+> pprExpr body <> semi

instance Ppr Literal where
  ppr = pprLiteral

pprLiteral :: Literal -> Doc
pprLiteral lit = case lit of
  LString s -> text (show s)
  LInt i -> int i
  LChar c -> char c
  LFloat d -> double d

