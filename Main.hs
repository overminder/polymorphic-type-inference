import Text.PrettyPrint
import qualified Data.Map as Map

import Parser
import Core
import ToCore
import Type
import TyCheck

env = Map.fromList [("+", TyLam [] (intType `tyArr` intType `tyArr` intType))]

main = do
  src <- getContents
  let prog = readProgram src
      e = toCore prog
  --print (pprExpr e)
  case runTypeCheck env e of
    Left err -> print err
    Right (env, et) -> do
      putStr "subst env: "
      putStrLn (showSubstEnv env)
      print (pprExpr et)

