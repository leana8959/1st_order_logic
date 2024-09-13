module Solver where

import Data.Map.Strict qualified as M
import Data.Set qualified as S

import Types (Formula (..), Ident, Valuation)

-- | Générer toutes les valuations possible (ensemble `Val`)
valuations :: [Ident] -> [Valuation]
valuations ps = M.fromList . zip ps <$> bools ps
  where
    -- Note: use ps as a counter
    bools [] = [[]]
    bools (_ : xs) = [p : t | p <- [True, False], t <- bools xs]

-- | Trouver toutes les propositions
freeVars :: Formula -> [Ident]
freeVars = S.toList . go
  where
    go (Var p) = S.singleton p
    go (Not f) = go f
    go (And f1 f2) = S.union (go f1) (go f2)
    go (Or f1 f2) = S.union (go f1) (go f2)
    go (Implies f1 f2) = S.union (go f1) (go f2)
    go _ = S.empty

-- | Evaluer une formule étant donné une valuation
eval :: Formula -> Valuation -> Bool
eval f vs = case f of
  Top -> True
  Bottom -> False
  Var p -> vs M.! p
  Not h -> not (eval h vs)
  And h g -> eval h vs && eval g vs
  Or h g -> eval h vs || eval g vs
  Implies h g -> not (eval h vs) || eval g vs

-- | Trouver toutes les valuations qui satisfait une formule
solve :: Formula -> [Valuation]
solve f = filter (eval f) $ valuations (freeVars f)

showSolution :: Valuation -> Int -> String
showSolution v i =
  unlines $
    ("solution nº" ++ show i) : ((\(var, val) -> var ++ ": " ++ show val) <$> M.toList v)

showSolutions :: [Valuation] -> String
showSolutions vs = unlines $ uncurry showSolution <$> zip vs [1 ..]
