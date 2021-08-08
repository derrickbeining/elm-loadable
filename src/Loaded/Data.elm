module Loaded.Data exposing (LoadedData(..))

{-| `LoadedData` models data which...

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  Cannot fail to load
5.  May reload


# Model

@docs LoadedData

-}

import Loaded.Progress exposing (Progress)
import Loaded.ReloadStatus exposing (ReloadStatus)


{-| -}
type LoadedData a
    = Pending Progress
    | Done ReloadStatus a
