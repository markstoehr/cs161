{- |
  Module      :  NState
  Copyright   :  (c) 2013, Stuart A. Kurtz
  License     :  BSD (3-Clause) (see ./LICENSE)
  
  Maintainer  :  stuart@cs.uchicago.edu
  Stability   :  experimental
 
  A nondeterministic state monad.
-}

module NState
    ( NState
    , NState.get
    , NState.put
    , NState.state
    , nstate
    , NState.modify
    , branch
    , runNState
    , evalNState
    , execNState
    ) where

import Control.Monad.State

{- |
  A non-deterministic state monad parameterized by the type s of the state to carry.
-}

type NState s = StateT s []

{- |
  Return the state from the internals of the monad.
-}

get :: NState s s
get = Control.Monad.State.get

{- |
  Replace the state inside the monad.
-}

put :: s -> NState s ()
put = Control.Monad.State.put

{- |
  Embed a simple state action into the monad.
-}

state :: (s -> (a,s)) -> NState s a
state = Control.Monad.State.state

{- |
  Embed a non-deterministic function state function, realized via the type
  @s -> [(a,s)]@, into the "NState" monad.
-}

nstate :: (s -> [(a,s)]) -> NState s a
nstate = StateT

{- |
  Maps an old state to a new state inside a state monad. The old state is thrown away.
-}

modify :: (s -> s) -> NState s ()
modify = Control.Monad.State.modify

{- |
  Non-deterministically select a value from the list of alternatives.
-}

branch :: [a]            -- ^ list of alternatives
          -> NState s a  -- ^ nondeterministic state computation
branch = lift

{- |
    Unwrap an "NState" computation as a function.
-}

runNState :: NState s a  -- ^ nondeterministic state computation
          -> s          -- ^ initial state
          -> [(a,s)]    -- ^ list of return values and final states
runNState = runStateT


{- |
  Evaluate a non-deterministic state computation with the given initial
  state and return the list of final values, discarding the final states.
-}

evalNState :: NState s a  -- ^ nondeterministic state computation
           -> s           -- ^ initial state
           -> [a]         -- ^ list of return values
evalNState = evalStateT

{- |
  Evaluate a non-deterministic state computation with the given initial
  state and return the list of final states, discarding the final vailes.
-}

execNState :: NState s a  -- ^ nondeterministic state computation
           -> s           -- ^ initial state
           -> [s]         -- ^ list of return states
execNState = execStateT
