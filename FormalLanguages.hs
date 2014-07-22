{- FormalLanguages.hs

Defines some basic notions of the formal language theory.-}

module FormalLanguages ( Symbol
                       , Word
                       , Language
                       ) where

type Symbol = String
type Word = [Symbol]
type Language = [Word]
