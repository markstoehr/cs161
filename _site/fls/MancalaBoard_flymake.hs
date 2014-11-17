module MancalaBoard (MancalaBoard, Player, initial, getCurPlayer,
            getBoardData, numCaptured, move, allowedMoves, isAllowedMove,
            gameOver, winners) where

import Data.List as List -- for List.elemIndex
import Data.Maybe as Maybe -- for List.elemIndex

{-
 - The stones on a Mancala board are simply recorded as a list of Ints.  The
 -  Ints come in the following order:
 - 1. The boardSize pits belonging to PlayerA
 - 2. The store belonging to PlayerA
 - 3. The boardSize pits belonging to PlayerB
 - 4. The store belonging to PlayerB
 -}

data MancalaBoard = MancalaBoardImpl [Int] Player

data Player = PlayerA | PlayerB deriving (Eq, Show)

---- Functions/constants for Player ----

allPlayers = [PlayerA, PlayerB]
numPlayers = length allPlayers


playerNum :: Player -> Int
playerNum p = fromJust $ List.elemIndex p allPlayers


playerWithNum :: Int -> Player
playerWithNum i = allPlayers !! i


nextPlayer :: Player -> Player
{- Find the player whose turn is next -}
nextPlayer p = playerWithNum $ ((playerNum p) + 1) `mod` numPlayers


---- Functions/constants for MancalaBoard ----

{- number of pits on each side -}
boardSize = 6
{- number of stones in each pit -}
startStones = 4

{- the initial mancala board -}
initial :: MancalaBoard
initial = MancalaBoardImpl (concat $ take numPlayers (repeat boardSide)) PlayerA
                        -- One side of board                pit at end
    where boardSide = take boardSize (repeat startStones) ++ [0]


{- return the index of the first pit belonging to a player -}
indexForFirstPit :: Player -> Int
indexForFirstPit p = (playerNum p) * (boardSize + 1)


{- return the index of the store for that player -}
indexForPlayerStore :: Player -> Int
indexForPlayerStore p = boardSize + (indexForFirstPit p)


{- return the indices for the pits (without the store) for a player -}
indicesForPlayerSide :: Player -> [Int]
indicesForPlayerSide p = [firstPit .. lastPit] where
    firstPit = indexForFirstPit p
    lastPit = firstPit + boardSize - 1


---- Retrieve information about Mancala Board
-- TODO: uncomment these type declarations and implement the functions
{- return the player who has the current turn -}
getCurPlayer :: MancalaBoard -> Player
--TODO: replace below line with real definition
getCurPlayer _ = PlayerA


{- return the list of all pits in the board -}
getBoardData :: MancalaBoard -> [Int]
--TODO: replace below line with real definition
getBoardData _ = []


{- return the side of the board for a specified player, including the store at
 - the end -}
--TODO: define this function
-- playerSide :: MancalaBoard -> Player -> [Int]


{- return the number of captured pieces in specified player's store -}
--TODO: add type and replace below line with real definition
numCaptured _ _ = 0


{- allowedMoves returns a list of valid moves for the current player:
 - ie. the indices of pits which belong to that player, and which contain one
 - or more pieces -}
--TODO: add type and replace below line with real definition
allowedMoves _ _ = []


{- check that a move is valid for the current player -}
--TODO: add type and replace below line with real definition
isAllowedMove _ _ = False


{- We number the pits from 0 to 13 (2 players, 6 pits each and 1 store each)
 - This function takes a board and applies the move where the player selects
 - the numbered pit, giving back an updated board after the move -}
--TODO: add type and replace below line with real definition
move _ _ = initial


{- gameOver checks to see if the game is over (i.e. if one player's side of the
 - board is all empty -}
gameOver :: MancalaBoard -> Bool
-- TODO: replace below line with real definition
gameOver _ = False


{- winner returns a list of players who have the top score: there will only be 
 - one in the list if there is a clear winner, and none if it is a draw -}
winners :: MancalaBoard -> [Player]
winners _ = allPlayers


---- show
instance Show MancalaBoard where
    show (MancalaBoardImpl boardData player) =
            (show boardData) ++ " " ++ (show player)

