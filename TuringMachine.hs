{- TuringMachine.hs

Contains the definition of the Turing machine as well as a description
of a universal Turing machine by Yu. Rogozhin.-}

module TuringMachines ( State
                      , MoveDirection
                      , TransitionFunc
                      , TuringMachine
                      , utm46
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

-- | The 4-state, 6-symbol universal Turing machine (Yu. Rogozhin).
--
-- Yurii Rogozhin, Small universal Turing machines, Theoretical
-- Computer Science, Volume 168, Issue 2, 20 November 1996, Pages
-- 215-240, ISSN 0304-3975.
utm46 :: TuringMachine
utm46 = TuringMachine [1..4] ["1", "b", "b>", "b<", "0", "c"] "0" 1 $ Map.fromList
        [ ( (1, "1" ),    ("b<", MoveLeft,  1) )
        , ( (1, "b" ),    ("b>", MoveRight, 1) )
        , ( (1, "b>"),    ("b",  MoveLeft,  1) )
        , ( (1, "b<"),    ("0",  MoveRight, 1) )
        , ( (1, "0" ),    ("b<", MoveLeft,  1) )
        , ( (1, "c" ),    ("0",  MoveRight, 4) )

        , ( (2, "1" ),    ("0",  MoveRight, 2) )
        , ( (2, "b" ),    ("b>", MoveLeft,  3) )
        , ( (2, "b>"),    ("b<", MoveRight, 2) )
        , ( (2, "b<"),    ("b>", MoveLeft,  2) )
        , ( (2, "0" ),    ("1",  MoveLeft,  2) )
        , ( (2, "c" ),    ("b",  MoveRight, 2) )

        , ( (3, "1" ),    ("1",  MoveRight, 3) )
        , ( (3, "b" ),    ("b<", MoveRight, 4) )
        , ( (3, "b>"),    ("b",  MoveRight, 3) )
--      , ( (3, "b<"),           HALT          )
        , ( (3, "0" ),    ("c",  MoveRight, 1) )
        , ( (3, "c" ),    ("1",  MoveRight, 1) )

        , ( (4, "1" ),    ("0",  MoveRight, 4) )
        , ( (4, "b" ),    ("c",  MoveLeft,  2) )
        , ( (4, "b>"),    ("b<", MoveRight, 4) )
--      , ( (4, "b<"),           HALT          )
        , ( (4, "0" ),    ("c",  MoveLeft,  2) )
        , ( (4, "c" ),    ("b",  MoveRight, 4) )
        ]
