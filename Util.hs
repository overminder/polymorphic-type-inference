module Util (
  module Text.PrettyPrint,
  Ppr(..),
) where

import Text.PrettyPrint
import Data.List (intersperse)

class Ppr a where
  ppr :: a -> Doc
  pprShow :: a -> String

  pprShow = show . ppr

instance Ppr a => Ppr [a] where
  ppr xs = brackets (hcat (intersperse comma (map ppr xs)))

