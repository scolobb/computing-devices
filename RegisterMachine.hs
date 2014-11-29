{- RegisterMachine.hs

Defines the register machine as well as some universal register
machines by I. Korec.-}

module RegisterMachine ( State
                       , Register
                       , Instruction(..)
                       , Program
                       , RegisterMachine(..)
                       , u22
                       , u20
                       , registerUse
                       , printInstr
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

-- | A (weakly) universal register machine with 20 instructions of
-- types /RiP/ and /RiZM/.
--
-- Ivan Korec: Small Universal Register
-- Machines. Theor. Comput. Sci. 168(2): 267-301 (1996).
u20 :: RegisterMachine
u20 = RegisterMachine [0,1,2,4,5,6,7] $ IntMap.fromList
      -- Instruction reader.
      [ (  1,   RiZM 1    3  6 )
      , (  3,   RiP  7    1    )
      , (  4,   RiZM 5    6  7 )
      , (  6,   RiP  6    4    )
      , (  7,   RiZM 6    9  4 )
      , (  9,   RiP  5   10    )
      , ( 10,   RiZM 7   12 13 )
      , ( 12,   RiP  1    7    )
      , ( 13,   RiZM 6   33  1 )
      , ( 33,   RiP  6   14    )
      , ( 14,   RiZM 4    1 16 )

        -- Decoder.
      , ( 16,   RiZM 5   18 23 )
      , ( 18,   RiZM 5   20 27 )
      , ( 20,   RiZM 5   22 30 )
      , ( 22,   RiP  4   16    )

        -- Simulation block.
      , ( 23,   RiZM 0   32  1 )
      , ( 27,   RiZM 2   32  1 )
      , ( 30,   RiP  0   31    )
      , ( 31,   RiP  2   32    )
      , ( 32,   RiZM 4    1 34 )

      , ( 34,   HALT           )
      ]

-- | Calculates the number of times a register is used in RiP commands
-- and RiZM commands respectively.
registerUse :: RegisterMachine -> Register -> (Int, Int)
registerUse (RegisterMachine _ prog) reg =
  foldl (\(ripUse, rizmUse) instr -> case instr of
            (RiP r _) -> if r == reg
                         then (ripUse + 1, rizmUse)
                         else (ripUse, rizmUse)
            (RiZM r _ _) -> if r == reg
                            then (ripUse, rizmUse + 1)
                            else (ripUse, rizmUse)
            HALT -> (ripUse, rizmUse)
          )(0, 0) $ IntMap.elems prog

-- | Breaks the list into groups of the given length.
takeBy :: Int -> [a] -> [[a]]
takeBy n xs@(_:_) | n > 0 = let (p,rest) = splitAt n xs in p : takeBy n rest
takeBy _ _ = []

-- | Prints a register machine instruction in LaTeX format.
printInstr :: State -> Instruction -> String
printInstr p (RiP r q) = "(q_" ++ (show p) ++ ", R" ++ (show r)
                               ++ "P, q_" ++ (show q) ++ ")"
printInstr p (RiZM r q q') = "(q_" ++ (show p) ++ ", R" ++ (show r)
                                   ++ "ZM, q_" ++ (show q) ++ ", q_" ++ (show q') ++ ")"
printInstr p HALT = "(q_" ++ (show p) ++ ", Stop)"
