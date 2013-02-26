{-# LANGUAGE DeriveFunctor #-}

module Core (
  Expr(..), Name, IsRec,
  Binding(..),
  annotationOf,

  InlineTypePpr(..),
  pprExpr, pprBinding,
) where

import Text.PrettyPrint

type Name = String
type IsRec = Bool
type Binding a = (Name, [Name], Expr a)

-- a: the annotation
data Expr a
  = EVar a Name
  | ELet a IsRec [Binding a] (Expr a)
  | EInt a Int
  | EFloat a Double
  | ELam a [Name] (Expr a)
  | EAp a (Expr a) (Expr a)
  deriving (Show, Functor)

annotationOf :: Expr a -> a
annotationOf e = case e of
  EVar a _ -> a
  ELet a _ _ _ -> a
  EInt a _ -> a
  EFloat a _ -> a
  ELam a _ _ -> a
  EAp a _ _ -> a

class InlineTypePpr a where
  pprAsBinding :: a -> Doc -> Doc
  pprAsVar :: a -> Doc -> Doc

instance InlineTypePpr () where
  pprAsBinding _ _ = empty
  pprAsVar _ doc = doc

pprExpr :: InlineTypePpr a => Expr a -> Doc
pprExpr e = case e of
  EVar a name -> pprAsVar a (text name)
  ELet a isRec bindings body ->
    text ("let" ++ (if isRec then "rec" else "")) $$ nest 2
      (vcat (map pprBinding bindings)) $$
      text "in" <+> pprExpr body
  EInt a i -> pprAsVar a (int i)
  EFloat a f -> pprAsVar a (double f)
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

pprBinding :: InlineTypePpr a => Binding a -> Doc
pprBinding (name, args, body) =
  pprAsBinding (annotationOf body) (text name) $$
  text name <+> hsep (map text args) <+> equals <+> pprExpr body <> semi

