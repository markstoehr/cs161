module Main where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.List (intersect, nub, foldl, union, intercalate, sort, group)
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Maybe

import NState

type Element = String
type Set = [Element]
type Collection = [Set]
type Cover = Map Element Set
-- data Problem = Problem { unused :: Collection
--                        , assignment :: Cover
--                        , uncovered :: Set
--                        }
type Solver = Cover -> [Cover]

extend :: Collection -> Element -> NState Cover ()
extend col e = undefined

allInserts :: Set -> Set -> Cover -> Cover
allInserts = undefined

viable :: Collection -> Element -> Cover -> Collection
viable = undefined

backtrack :: Collection -> Solver
backtrack = undefined

universe :: Collection -> [Element]
universe = undefined

makeCollection :: String -> Collection
makeCollection = undefined

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines


runSolver :: Collection -> String
runSolver collection = case backtrack collection Map.empty of
    [x] -> (++) "1 solution\n" $ show x
    []  -> "0 solutions"
    xs  -> (((++) $ show $ length xs) " solutions\n") ++ (intercalate "\n" . map show $ xs)


main :: IO ()
main = do
    collection <- fmap makeCollection getContents
    putStr . unlines $
        [ "problem:"
        , ""
        , indent 3 $ (++) "Elements: " $ show $ universe collection
        , indent 3 $ (++) "Sets: " $ show collection
        , ""
        , "solutions:"
        , ""
        , indent 3 $ runSolver collection
        ]

