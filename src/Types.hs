module Types where

import Data.Map.Strict qualified as M

-- | Free variable
type Ident = String

-- | Given a variable, the association of whether it's True of False
type Valuation = M.Map Ident Bool

data Formula
  = Bottom
  | Top
  | Var Ident
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  deriving (Show, Eq)
