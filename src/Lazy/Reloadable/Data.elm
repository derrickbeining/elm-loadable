module Lazy.Reloadable.Data exposing (LazyReloadableData(..))

{-| `LazyReloadableData` models data which...

1.  Needs to be loaded
2.  Does NOT begin loading immediately and may never begin to load but only when required
3.  May load incrementally
4.  Cannot fail to load
5.  May reload


# Model

@docs LazyReloadableData

-}

import Loaded.Progress exposing (Progress)
import Loaded.ReloadStatus exposing (ReloadStatus)


{-| -}
type LazyReloadableData value
    = Initial
    | Pending Progress
    | Done ReloadStatus value
