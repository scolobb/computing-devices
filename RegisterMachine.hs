{- RegisterMachine.hs

Defines the register machine as well as some universal register
machines by I. Korec.-}

module RegisterMachine ( State
                       , Register
                       , Instruction(..)
                       , Program
                       , RegisterMachine(..)
                       ) where

import qualified Data.IntMap as IntMap

type State = Int
type Register = Int

-- | Some instruction types of the register machine.
data Instruction
   -- | Increment the register and go to that state.
  = RiP Register State
    -- | Try decrementing the register; if successful, go to the first
    -- state.  Otherwise, go to the second state.
  | RiZM Register State State
  deriving (Show, Read, Eq, Ord)

type Program = IntMap.IntMap Instruction

data RegisterMachine = RegisterMachine [Register] Program
                 deriving (Show, Read, Eq, Ord)
