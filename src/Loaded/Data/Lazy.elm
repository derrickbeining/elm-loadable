module Loaded.Data.Lazy exposing (LazyLoadedData(..))

{-| `LazyLoadedData` models data which...

1.  Needs to be loaded
2.  Does not begin loading immediately but only when required
3.  May load incrementally
4.  Cannot fail to load
5.  May reload


# Model

@docs LazyLoadedData

-}

import Loaded.Progress exposing (Progress)
import Loaded.ReloadStatus exposing (ReloadStatus)


{-| -}
type LazyLoadedData a
    = Initial
    | Pending Progress
    | Done ReloadStatus a
