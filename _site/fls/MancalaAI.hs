module MancalaAI(aiNextMove) where

import MancalaBoard

type Move = Int

-- Decides what move to make for the current player of the specified MancalaBoard.
aiNextMove :: MancalaBoard -> Move
aiNextMove mancala = head $ allowedMoves mancala

simulateGame :: (MancalaBoard -> Move) -> (MancalaBoard -> Move) -> MancalaBoard -> Int -> MancalaBoard
simulateGame = undefined