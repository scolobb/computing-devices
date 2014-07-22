{- FormalLanguages.hs

Defines some basic notions of the formal language theory.-}

module FormalLanguages ( Symbol
                       , Word
                       , Language
                       ) where

import qualified Data.Set as Set

type Symbol = String
type Word = [Symbol]
type Language = Set.Set Word
