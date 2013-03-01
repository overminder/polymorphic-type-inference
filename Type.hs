{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Type (
  Kind(..), HasKind(..),

  Type(..),
  TyVar(..), HasTyVar(..),
  TyCon(..), TyId(..),
  mkTyVar, mkTyCon, mkTyGen,

  unitType, charType, intType, boolType,
  floatType, listType, arrowType,
  stringType, tuple2Type,

  tyArr, apTyCon, pairTypeWith, mkListType,

  Subst, nullSubst, (+->), (@@), merge,
  Qual(..), Pred(..), Scheme(..), quantify, toScheme, Assump(..),

  pprKind, pprType, pprSubst, pprAssump, pprScheme,
) where

import Util

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

data Kind
  = KStar
  | KArrow Kind Kind
  deriving (Show, Eq, Ord)

infixr 5 `KArrow`

newtype TyId = TyId { unTyId :: Int }
  deriving (Eq, Ord)

instance Show TyId where
  show = show . unTyId

data Type
  = TyVar TyVar
  | TyCon TyCon
  | TyApp Type Type
  | TyGen TyId
  deriving (Show, Eq, Ord)

newtype TyVar = MkTyVar { unTyVar :: (TyId, Kind) }
  deriving (Show, Eq, Ord)

mkTyVar i k = TyVar (MkTyVar (TyId i, k))

newtype TyCon = MkTyCon { unTyCon :: (String, Kind) }
  deriving (Show, Eq, Ord)

mkTyCon name k = TyCon (MkTyCon (name, k))

mkTyGen :: Int -> Type
mkTyGen = TyGen . TyId

unitType = mkTyCon "()" KStar
charType = mkTyCon "Char" KStar
intType = mkTyCon "Int" KStar
boolType = mkTyCon "Bool" KStar
floatType = mkTyCon "Float" KStar
arrowType = mkTyCon "->" (KStar `KArrow` KStar `KArrow` KStar)
listType = mkTyCon "[]" (KStar `KArrow` KStar)
stringType = TyApp listType charType
tuple2Type = mkTyCon "," (KStar `KArrow` KStar `KArrow` KStar)

infixr 6 `tyArr`
tyArr x y = apTyCon arrowType [x, y]

infix 6 `pairTypeWith`
pairTypeWith x y = apTyCon tuple2Type [x, y]

mkListType x = apTyCon listType [x]

apTyCon :: Type -> [Type] -> Type
apTyCon = foldl TyApp

class HasKind t where
  kind :: t -> Kind

instance HasKind TyVar where
  kind = snd . unTyVar

instance HasKind TyCon where
  kind = snd . unTyCon

instance HasKind Type where
  kind ty = case ty of
    TyVar tv -> kind tv
    TyCon tc -> kind tc
    TyApp t1 _ -> case kind t1 of
                    KArrow _ res -> res

-- Type manipulation

-- XXX: how is this different from, say, using the associative-list approach?
type Subst = Map TyVar Type

nullSubst = Map.empty

infix 5 +->
(+->) :: TyVar -> Type -> Subst
(+->) = Map.singleton

-- Forall s1 s2. subst the value of s2 by s1 and merge s2 with s1
-- If there exists conflict, the result will be biased towards s1
infixr 4 @@
(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = Map.foldrWithKey combine s1 s2
  where
    combine k v res = Map.insert k (applySubst s1 v) res

merge :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 `Map.union` s2) else fail "merge fails"
  where
    agree = all (\v -> applySubst s1 (TyVar v) == applySubst s2 (TyVar v))
                (Map.keys s1 `List.intersect` Map.keys s2)


class HasTyVar t where
  applySubst :: Subst -> t -> t
  tyVars :: t -> [TyVar]

instance HasTyVar Type where
  applySubst s t@(TyVar u) = case Map.lookup u s of
                             Just t' -> t'
                             Nothing -> t
  applySubst s (TyApp l r) = TyApp (applySubst s l) (applySubst s r)
  applySubst s t           = t

  tyVars (TyVar u)         = [u]
  tyVars (TyApp l r)       = tyVars l `List.union` tyVars r
  tyVars _                 = []

instance HasTyVar t => HasTyVar [t] where
  applySubst = map . applySubst
  tyVars = List.nub . concatMap tyVars

-- Typeclasses
data Qual t = [Pred] :=> t
  deriving (Show, Eq, Ord)

data Pred = IsIn String Type
  deriving (Show, Eq, Ord)

instance HasTyVar t => HasTyVar (Qual t) where
  applySubst s (ps :=> t) = applySubst s ps :=> applySubst s t
  tyVars       (ps :=> t) = tyVars ps `List.union` tyVars t

instance HasTyVar Pred where
  applySubst s (IsIn i t) = IsIn i (applySubst s t)
  tyVars       (IsIn i t) = tyVars t

-- Type schemes
data Scheme = Forall [Kind] (Qual Type)
  deriving (Eq)

instance HasTyVar Scheme where
  applySubst s (Forall ks qt) = Forall ks (applySubst s qt)
  tyVars (Forall ks qt)       = tyVars qt

-- Make a generic type scheme
quantify :: [TyVar] -> Qual Type -> Scheme
quantify genTyVars concreteType
  = Forall usedKinds (applySubst tv2gen concreteType)
  where
    tv2gen = Map.fromList (zip usedGenTyVars (map mkTyGen [0..]))
    usedKinds = map kind usedGenTyVars
    usedGenTyVars = [tv | tv <- tyVars concreteType, tv `elem` genTyVars]

-- Simple scheme
toScheme :: Type -> Scheme
toScheme = Forall [] . ([] :=>)

-- Assumptions, or gamma environment
newtype Assump = Assump { unAssump :: Map String Scheme }

instance HasTyVar Assump where
  applySubst s = Assump . Map.map (applySubst s) . unAssump
  tyVars = List.nub . concatMap tyVars . Map.elems . unAssump

-- ppr
instance Ppr Kind where
  ppr = pprKind

pprKind :: Kind -> Doc
pprKind k = case k of
  KStar -> text "*"
  KArrow _ _ -> pprKArrow False k
  where
    pprKArrow needParen k = case k of
      KArrow k1 k2 -> mbAddParen
        (pprKArrow True k1 <+> text "->" <+> pprKArrow False k2)
      _ -> pprKind k
      where
        mbAddParen = if needParen then parens else id

instance Ppr Type where
  ppr = pprType

pprType :: Type -> Doc
pprType ty = case ty of
  TyVar tv -> ppr tv
  TyCon (MkTyCon (name, k)) -> case k of
    KStar -> text name
    _ -> parens (text name)
  TyApp t1 t2 -> pprTyApp False ty
  TyGen (TyId i) -> char (['a'..'z'] !! i)

instance Ppr TyVar where
  ppr (MkTyVar (TyId i, _)) = text "t" <> int i

pprTyApp :: Bool -> Type -> Doc
pprTyApp needParen ty = case toList ty of
  TyCon (MkTyCon (name, kind)):tys@(a1:_) ->
    case name of
      -- Some special cases
      "[]" -> brackets (pprType a1)
      "->" -> let [a1, a2] = tys
               in mbAddParen (pprTyApp True a1 <+> text "->" <+>
                              pprTyApp False a2)
      "," -> let [a1, a2] = tys
              in parens (pprType a1 <> comma <+> pprType a2)
      _ -> mbAddParen (text name <+> hsep (map (pprTyApp True) tys))

  _:_ -> pprType ty
  where
    toList t = case t of
      TyApp t1 t2 -> toList t1 ++ [t2]
      _ -> [t]

    mbAddParen = if needParen then parens else id

instance Ppr Subst where
  ppr = pprSubst

pprSubst :: Subst -> Doc
pprSubst s = braces (vcat (map pprEntry (Map.toList s)))
  where
    pprEntry (tv, t) = pprType (TyVar tv) <+> equals <+> pprType t

instance Ppr Assump where
  ppr = pprAssump

pprAssump :: Assump -> Doc
pprAssump (Assump dct) = braces (vcat (map pprEntry (Map.toList dct)))
  where
    pprEntry (name, scheme)
      = text name <+> text "::" <+> pprScheme scheme

instance Ppr Scheme where
  ppr = pprScheme

pprScheme :: Scheme -> Doc
pprScheme (Forall ks (ps :=> t)) = pprType t

