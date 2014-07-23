{- RegisterMachine.hs

Defines the register machine as well as some universal register
machines by I. Korec.-}

module RegisterMachine ( State
                       , Register
                       , Instruction(..)
                       , Program
                       , RegisterMachine(..)
                       , u22
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
  | HALT
  deriving (Show, Read, Eq, Ord)

type Program = IntMap.IntMap Instruction

data RegisterMachine = RegisterMachine [Register] Program
                 deriving (Show, Read, Eq, Ord)

-- | A (strongy) universal register machine with 22 instructions of
-- types /RiP/ and /RiZM/.
--
-- Ivan Korec: Small Universal Register
-- Machines. Theor. Comput. Sci. 168(2): 267-301 (1996).
u22 :: RegisterMachine
u22 = RegisterMachine [0..7] $ IntMap.fromList
      -- Instruction reader.
      [ (  1,   RiZM 1    3  6 )
      , (  3,   RiP  7    1    )
      , (  4,   RiZM 5    6  7 )
      , (  6,   RiP  6    4    )
      , (  7,   RiZM 6    9  4 )
      , (  9,   RiP  5   10    )
      , ( 10,   RiZM 7   12 13 )
      , ( 12,   RiP  1   7     )
      , ( 13,   RiZM 6   33  1 )
      , ( 33,   RiP  6   14    )
      , ( 14,   RiZM 4   1  16 )

        -- Decoder.
      , ( 16,   RiZM 5   18 23 )
      , ( 18,   RiZM 5   20 27 )
      , ( 20,   RiZM 5   22 30 )
      , ( 22,   RiP  4   16    )

        -- Simulation block.
      , ( 23,   RiZM 2   32 25 )
      , ( 25,   RiZM 0    1 32 )
      , ( 27,   RiZM 3   32 29 )
      , ( 29,   RiP  0    1    )
      , ( 30,   RiP  2   31    )
      , ( 31,   RiP  3   32    )
      , ( 32,   RiZM 4    1 34 )

      , ( 34,   HALT           )
      ]
