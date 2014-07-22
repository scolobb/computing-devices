{- TuringMachine.hs

Contains the definition of the Turing machine as well as a description
of a universal Turing machine by Yu. Rogozhin.-}

module TuringMachines ( State
                      , MoveDirection
                      , TransitionFunc
                      , TuringMachine
                      ) where

import FormalLanguages
import qualified Data.Map as Map

type State = Int

data MoveDirection = MoveLeft | MoveRight
                   deriving (Show, Read, Eq, Ord)

-- | The transition function of the Turing machine.
type TransitionFunc = Map.Map (State, Symbol) (Symbol, MoveDirection, State)

-- | The Turing machine.
--
-- The input alphabet is not given explicitly.
data TuringMachine = TuringMachine
                     [State]          -- ^ The states of the Turing machine.
                     [Symbol]         -- ^ The alphabet of symbols.
                     Symbol           -- ^ The blank symbol.
                     State            -- ^ The initial state.
                     TransitionFunc   -- ^ The transition function.
                   deriving (Show, Read, Eq, Ord)
