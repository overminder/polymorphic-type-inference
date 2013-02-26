module TyInterf where

import Control.Monad.RWS
import Data.Map (Map)
import qualified Data.Map as Map

import Type
import Core

-- Type interference state
data TiState
  = TiState {
    subst :: Subst,
    uniqueSource :: Int
  }
  deriving (Show)

type TiM = RWS Assump () TiState

runTi :: Assump -> TiM a -> a
runTi gamma m = fst (evalRWS m gamma emptyState)
  where
    emptyState = TiState {
      subst = nullSubst,
      uniqueSource = 0
    }

unify :: Type -> Type -> TiM ()
unify t1 t2 = do
  s <- gets subst
  u <- mgu (applySubst s t1) (applySubst s t2)
  extendSubst u

extendSubst :: Subst -> TiM ()
extendSubst s' = modify $ \st -> st {
  subst = s' @@ subst st
}

newTyVar :: Kind -> TiM Type
newTyVar k = do
  i <- gets uniqueSource
  modify $ \st -> st { uniqueSource = i + 1 }
  return $ mkTyVar i k

freshInst :: Scheme -> TiM (Qual Type)
freshInst (Forall ks qt) = do
  ts <- mapM newTyVar ks
  return $ inst ts qt

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TyApp l r) = TyApp (inst ts l) (inst ts r)
  inst ts (TyGen n)   = ts !! unTyId n
  inst _  t           = t

instance Instantiate a => Instantiate [a] where
  inst = map . inst

instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)

-- Find a most general unification s.t. apply subst t1 == apply subst t2
-- See Unification@Wikipedia, "Examples of syntatic unification of
--                             first-order terms"
mgu :: Monad m => Type -> Type -> m Subst
varBind :: Monad m => TyVar -> Type -> m Subst

mgu (TyApp l r) (TyApp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (applySubst s1 r) (applySubst s1 r')
  return (s2 @@ s1)

mgu (TyVar u) t = varBind u t
mgu t (TyVar u) = varBind u t

mgu (TyCon tc1) (TyCon tc2)
  | tc1 == tc2 = return nullSubst

mgu t1 t2 = fail $ "cannot unify " ++ show (pprType t1) ++
                   " with " ++ show (pprType t2)

varBind u t
  | TyVar u == t      = return nullSubst
  | u `elem` tyVars t = fail $ "Recursive type: " ++ showU ++ " in " ++ showT
  | kind u /= kind t  = fail $ "Kinds mismatch: " ++ showU ++ " and " ++ showT
  | otherwise         = return (u +-> t)
  where
    showU = show (pprType (TyVar u))
    showT = show (pprType t)

-- Find an unification s.t. apply subst t1 == t2
match :: Monad m => Type -> Type -> m Subst
match (TyApp l r) (TyApp l' r') = do
  sl <- match l l'
  sr <- match r r'
  merge sl sr

match (TyVar u) t
  | kind u == kind t = return (u +-> t)

match (TyCon tc1) (TyCon tc2)
  | tc1 == tc2 = return nullSubst

match t1 t2 = fail $ "cannot unify " ++ show (pprType t1) ++
                     " with " ++ show (pprType t2)

-- Finally, the type inferencer!
type Infer e t = Assump -> e -> TiM ([Pred], t)
type InferredType = ([Pred], Type)

simpleType :: Type -> InferredType
simpleType t = ([], t)

typeOf :: Expr InferredType -> Type
typeOf = snd . annotationOf

predsOf :: Expr InferredType -> [Pred]
predsOf = fst  .annotationOf

tiExpr :: Expr () -> TiM (Expr InferredType)
tiExpr e = case e of
  EInt _ i -> return $ EInt (simpleType intType) i

  EVar _ name -> do
    mbScheme <- asks (Map.lookup name . unAssump)
    case mbScheme of
      Nothing -> fail $ "unbound identifier: " ++ name
      Just scheme -> do
        (ps :=> t) <- freshInst scheme
        return $ EVar (ps, t) name

  EAp _ e f -> do
    e' <- tiExpr e
    f' <- tiExpr f
    t <- newTyVar KStar
    unify (typeOf f' `tyArr` t) (typeOf e')
    return (EAp (predsOf f' ++ predsOf e', t) e' f')

  ELet _ bs e -> do
    bs' <- tiBindings bs
    e' <- local (updateAssump bs') $ tiExpr e
    return (ELet 

tiBindings :: Binding () -> TiM (Binding InferredType)
tiBindings (name, args, body) = do
  argTypes <- mapM (const (newTyVar KStar)) args

