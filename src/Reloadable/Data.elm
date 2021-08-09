module Reloadable.Data exposing (ReloadableData(..))

{-| `ReloadableData` models data which...

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  Cannot fail to load
5.  May reload


# Model

@docs ReloadableData

-}

import Loaded.Progress exposing (Progress)
import Loaded.ReloadStatus exposing (ReloadStatus)


{-| -}
type ReloadableData value
    = Pending Progress
    | Done ReloadStatus value
