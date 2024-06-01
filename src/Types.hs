module Types where

import Data.Map.Strict qualified as M
import Data.Text (Text)

-- | Free variable
type Var = Text

-- | Given a variable, the association of whether it's True of False
type Valuation = M.Map Var Bool

data Formula
  = Bottom
  | Top
  | Var Var
  | Not Formula
  | And Formula Formula
  | Or Formula Formula
  | Implies Formula Formula
  deriving (Show, Eq)
