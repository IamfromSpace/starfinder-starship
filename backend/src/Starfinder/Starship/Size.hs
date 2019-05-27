{-# LANGUAGE DeriveFunctor #-}
module Starfinder.Starship.Size (Size(..), topSpeed, Sized(..)) where


data Size
    = Tiny
    | Small
    | Medium
    | Large
    | Huge
    | Gargantuan
    | Colossal
    deriving (Show, Eq, Ord, Enum)


topSpeed :: Size -> Int
topSpeed size =
    case size of
        Tiny ->
            14

        Small ->
            12

        Medium ->
            12

        Large ->
            10

        Huge ->
            10

        _ ->
            8

data Sized a = Sized Size a deriving (Functor)
