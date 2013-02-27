import qualified Data.Map as Map

import Util
import Parser
import Core
import Type
import TyInterf

bltinAssump = Assump $ Map.fromList
  [ ("+", toScheme (intType `tyArr` intType `tyArr` intType))
  , ("chr", toScheme (intType `tyArr` charType))
  , ("fix", Forall [KStar]
              ([] :=> ((mkTyGen 0 `tyArr` mkTyGen 0) `tyArr` mkTyGen 0)))
  ]

main = do
  src <- getContents
  let prog = readProgram src
      (prog', _) = runTi bltinAssump (tiBindings prog (EVar () "main"))
  print (vcat (map ppr prog'))

