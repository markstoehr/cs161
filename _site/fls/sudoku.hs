module Main where

import Control.Monad
import Data.Char
import Data.List ((\\),replicate,intercalate,nub,delete)
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Maybe

import NState

type Position = (Int,Int)
type Board = Map Position Int
type Solver = Board -> [Board]

positions :: [Position]
positions = liftM2 (,) [1..9] [1..9]

backtrack :: Solver
backtrack = execNState (mapM_ extend positions) where
    extend pos = do
        board <- get
        val <- branch $ viable pos board
        put $ Map.insert pos val board
    viable pos board = candidates \\ conflicts where
        candidates = case Map.lookup pos board of
            Just x  -> [x]
            Nothing -> [1..9]
        conflicts = mapMaybe (`Map.lookup` board) $ neighbors pos

neighbors :: Position -> [Position]
neighbors = (Map.fromList [(p,neighborf p) | p <- positions] !) where
    neighborf pos = delete pos . nub . concatMap ($ pos) $ [rowOf,colOf,blockOf]
    rowOf (row,col) = map ((,) row)  [1..9]
    colOf (row,col) = map (flip (,) col) [1..9]
    blockOf (row,col) = [(trunc row + rx,trunc col + cx) | rx <- [1..3], cx <- [1..3]]
    trunc x = nx - nx `mod` 3 where nx = x-1

runSolver :: Solver -> Board -> String
runSolver solver board = case solver board of
    [x] -> format x
    []  -> "no solution"
    _   -> "multiple solutions"

format :: Board -> String
format board = concatMap fmt positions where
    fmt pos = entry pos ++ separator pos
    separator (row,col)
        | rem col 3 /= 0 = " "
        | col < 9        = "   "
        | rem row 3 /= 0 = "\n"
        | row < 9        = "\n\n"
        | otherwise      = "\n"
    entry pos = case Map.lookup pos board of
        Just x  -> show x
        Nothing -> "."

makeBoard :: String -> Board
makeBoard = Map.fromList . mapMaybe f . zip positions . words where
    f (p,s)
        | all isDigit s = Just (p,read s)
        | s `elem` [".","-","_"] = Nothing
        | otherwise = error $ "unrecognized character in input: " ++ s

indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

main :: IO ()
main = do
    board <- fmap makeBoard getContents
    putStr . unlines $
        [ "problem:"
        , ""
        , indent 3 (format board)
        , ""
        , "solution:"
        , ""
        , indent 3 (runSolver backtrack board)
        ]
