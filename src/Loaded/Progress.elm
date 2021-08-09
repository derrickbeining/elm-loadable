module Loaded.Progress exposing
    ( Progress(..), Percent
    , mkPercent
    , getPercent, percentToFloat
    , isUntracked, isPercentage
    )

{-|


# Model

@docs Progress, Percent


# Constructors

@docs mkPercent


# Destructors

@docs getPercent, percentToFloat


# Guards

@docs isUntracked, isPercentage

-}


{-| Represents how much of the data has been loaded.
-}
type Progress
    = Untracked
    | Percentage Percent


{-| An opaque type to represent progress percentage as a [Float](Basics#Float)
between `0` and `1` inclusive.
-}
type Percent
    = Percent Float


{-| -}
isUntracked : Progress -> Bool
isUntracked progress =
    case progress of
        Untracked ->
            True

        Percentage _ ->
            False


{-| -}
isPercentage : Progress -> Bool
isPercentage progress =
    case progress of
        Untracked ->
            False

        Percentage _ ->
            True


{-| A smart constructor for [Percent](#Percent). The provided
[Float](Basics#Float) should be between `0` and `1` inclusive. Any value
outside that range will be clamped to the nearest value in range.

        mkPercent -1 == mkPercent 0
        --> True

        mkPercent 0.5 == mkPercent 0.5
        --> True

        mkPercent 1.5 == mkPercent 1
        --> True

-}
mkPercent : Float -> Percent
mkPercent percent =
    Percent <|
        if percent < 0 then
            0

        else if percent > 1 then
            1

        else
            percent


{-| A smart accessor to safely read the progress percentage as a
[Float](Basics#Float) between `0` and `1` inclusive.
-}
getPercent : Progress -> Maybe Percent
getPercent progress =
    case progress of
        Untracked ->
            Nothing

        Percentage percentage ->
            Just percentage


{-| -}
percentToFloat : Percent -> Float
percentToFloat (Percent p) =
    p
