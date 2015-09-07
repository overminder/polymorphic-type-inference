module TyInf (
  runTi,
  tiExpr,
  tiBindings,
) where

import Util
import Type
import Core

import Control.Monad.RWS hiding ((<>))
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.List as List
import Debug.Trace (trace)

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
  traceM (show (text "unify" <+> pprType t1 <+> text "with" <+> pprType t2 <+>
                text "under" <+> pprSubst s $$
                text "produces" <+> pprSubst u <> semi))

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
type QType = Qual Type

simpleQual :: Type -> QType
simpleQual t = [] :=> t

typeOf :: HasAnnotation a => a QType -> Type
typeOf e = case annotationOf e of
  _ :=> t -> t

predsOf :: HasAnnotation a => a QType -> [Pred]
predsOf e = case annotationOf e of
  ps :=> _ -> ps

tiExpr :: Expr () -> TiM (Expr QType)
tiExpr e = traceM (show (text "tiExpr" <+> pprExpr e)) >> case e of
  ELit _ lit -> do
    qt <- tiLiteral lit
    return $ ELit qt lit

  EVar _ name -> do
    gamma <- ask
    let mbScheme = M.lookup name (unAssump gamma)
    case mbScheme of
      Nothing -> fail $ "unbound identifier: " ++ name
      Just scheme -> do
        (ps :=> t) <- freshInst scheme
        traceM (show (text "instantiate" <+> text name <+> text "in" <+>
                      pprAssump gamma <> comma <+> text "got" <+>
                      pprType t <> semi))
        return $ EVar (ps :=> t) name

  EAp _ e f -> do
    e' <- tiExpr e
    f' <- tiExpr f
    t <- newTyVar KStar
    unify (typeOf f' `tyArr` t) (typeOf e')
    return $ EAp ((predsOf f' ++ predsOf e') :=> t) e' f'

  ELet _ True bs e -> do
    (bs', e') <- tiBindings bs e
    return $ ELet (annotationOf e') True bs' e'

  ELet _ False bs e -> do
    inputGamma <- ask
    tiBinds <- mapM tiLetBinding bs
    currSubst <- gets subst
    let fixedTyVars = tyVars (applySubst currSubst inputGamma)
        letTyVars = tyVars (map typeOf tiBinds)
        genericTyVars = letTyVars List.\\ fixedTyVars
        schemes = map (quantify genericTyVars . annotationOf) tiBinds
        names = map bind_name tiBinds
        newGamma = Assump (M.fromList (zip names schemes))
        mkNewGamma gamma = foldr (uncurry M.insert) gamma (zip names schemes)
    local (Assump . mkNewGamma . unAssump) $ do
      gamma <- ask
      traceM (show (text "tiLetBody, got binders:" <+>
                    ppr tiBinds <> comma <+>
                    text "subst env =" <+>
                    ppr currSubst <> comma <+>
                    text "genTyVars =" <+>
                    ppr genericTyVars $$
                    text "and newGamma =" <+>
                    ppr newGamma <> semi))
      e' <- tiExpr e
      return $ ELet (annotationOf e') False tiBinds e'

-- polymorphic non-recursive let binding
tiLetBinding :: Binding () -> TiM (Binding QType)
tiLetBinding (Binding _ name args body) = do
  argTypes <- mapM (const (newTyVar KStar)) args
  let schemes = map toScheme argTypes
      mkNewGamma gamma = foldr (uncurry M.insert) gamma (zip args schemes)

  local (Assump . mkNewGamma . unAssump) $ do
    tiBody <- tiExpr body
    currSubst <- gets subst
    let actualTy = foldr tyArr (typeOf tiBody) argTypes
        -- XXX assume we don't have qual
        -- Newly constructed complex type should be substituted once
        actualTy' = simpleQual (applySubst currSubst actualTy)
    return $ Binding actualTy' name args tiBody

-- Typecheck recursive bindings
-- XXX: we need to separate non-related binding groups
tiBindings :: [Binding ()] -> Expr () -> TiM ([Binding QType],
                                              Expr QType)
tiBindings binds body = do
  -- Make a fresh tyVar for each binder name, and extend the current
  -- gamma with those vars, since the bindings are recursive.
  rhsTypes <- mapM (const (newTyVar KStar)) binds
  let names = map bind_name binds
      schemes = map toScheme rhsTypes
      mkNewGamma gamma = foldr (uncurry M.insert) gamma (zip names schemes)
  
  -- Apply the recursive subst back to rhss and gamma
  local (Assump . mkNewGamma . unAssump) $ do
    tiBinds <- sequence (zipWith tiBinding binds rhsTypes)
    recSubst <- gets subst
    gamma <- ask
    let tiBinds' = applySubst recSubst tiBinds
        rhsTypes' = applySubst recSubst rhsTypes
        vss = tyVars rhsTypes'
        fixedTyVars = tyVars (applySubst recSubst gamma)
        genericTyVars = vss List.\\ fixedTyVars
        schemes' = map (quantify genericTyVars . simpleQual) rhsTypes'
        mkNewGamma gamma = foldr (uncurry M.insert) gamma (zip names schemes')
  
    local (Assump . mkNewGamma . unAssump) $ do
      body' <- tiExpr body
      return (tiBinds', body')

tiBinding :: Binding () -> Type -> TiM (Binding QType)
tiBinding (Binding _ name args body) bindType = do
  argTypes <- mapM (const (newTyVar KStar)) args
  let schemes = map toScheme argTypes
      mkNewGamma gamma = foldr (uncurry M.insert) gamma (zip args schemes)

  local (Assump . mkNewGamma . unAssump) $ do
    tiBody <- tiExpr body
    let actualTy = foldr tyArr (typeOf tiBody) argTypes
    unify actualTy bindType
    -- XXX assume we don't have qual
    return $ Binding (simpleQual actualTy) name args tiBody

tiLiteral :: Literal -> TiM QType
tiLiteral lit = liftM simpleQual . return $ case lit of
  LString _ -> stringType
  LInt _ -> intType
  LChar _ -> charType
  LFloat _ -> floatType

-- Missing instance decl
instance HasTyVar a => HasTyVar (Binding a) where
  applySubst s = fmap (applySubst s)
  tyVars = List.nub . concatMap tyVars . toList

instance HasTyVar a => HasTyVar (Expr a) where
  applySubst s = fmap (applySubst s)
  tyVars = List.nub . concatMap tyVars . toList

instance InlineTypePpr QType where
  pprAsBinding (_ :=> t) doc = doc <+> text "::" <+> pprType t
  pprAsVar (_ :=> t) doc = parens (doc <+> text "::" <+> pprType t)

traceM s = --trace s $
  return undefined

