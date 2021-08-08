module Loaded.ReloadStatus exposing
    ( ReloadStatus(..)
    , isInert, isReloading
    )

{-|


# Model

@docs ReloadStatus


# Guards

@docs isInert, isReloading

-}

import Loaded.Progress exposing (Progress)


{-| Indicates whether or not the data is being reloaded
-}
type ReloadStatus
    = Inert
    | Reloading Progress


{-| Predicates that the status is `Inert`
-}
isInert : ReloadStatus -> Bool
isInert =
    (==) Inert


{-| Predicates that the status is `Reloading`
-}
isReloading : ReloadStatus -> Bool
isReloading =
    not << isInert
