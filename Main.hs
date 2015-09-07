import qualified Data.Map as Map

import Util
import Parser
import Core
import Type
import TyInf

-- some test assumptions
bltinAssump = Assump $ Map.fromList
  [ ("(+)", toScheme (intType `tyArr` intType `tyArr` intType))
  , ("(-)", toScheme (intType `tyArr` intType `tyArr` intType))
  , ("(<)", toScheme (intType `tyArr` intType `tyArr` boolType))
  , ("if#", Forall [KStar]
              ([] :=> (boolType `tyArr` mkTyGen 0 `tyArr`
                                        mkTyGen 0 `tyArr`
                                        mkTyGen 0)))
  , ("True", toScheme boolType)
  , ("False", toScheme boolType)
  , ("chr", toScheme (intType `tyArr` charType))
  , ("fix", Forall [KStar]
              ([] :=> ((mkTyGen 0 `tyArr` mkTyGen 0) `tyArr` mkTyGen 0)))
  , ("(,)", Forall [KStar, KStar]
              ([] :=> (mkTyGen 0 `tyArr` mkTyGen 1 `tyArr`
                        (mkTyGen 0 `pairTypeWith` mkTyGen 1))))
  , ("(:)", Forall [KStar]
              ([] :=> (mkTyGen 0 `tyArr` mkListType (mkTyGen 0) `tyArr` 
                         mkListType (mkTyGen 0))))
  , ("[]", Forall [KStar]
              ([] :=> mkListType (mkTyGen 0)))
  ]

main = do
  src <- getContents
  let prog = readProgram src
      (prog', _) = runTi bltinAssump (tiBindings prog (EVar () "main"))
  print (ppr prog)
  print (vcat (map ppr prog'))

