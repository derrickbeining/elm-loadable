module Lazy.Loaded.Data exposing (LazyLoadedData(..))

{-| `LazyLoadedData` models data which...

1.  Needs to be loaded
2.  Does NOT begin loading immediately and may never load but only when required
3.  May load incrementally
4.  Cannot fail to load
5.  Will _NOT_ reload


# Model

@docs LazyLoadedData

-}

import Loaded.Progress exposing (Progress)


{-| -}
type LazyLoadedData value
    = Initial
    | Pending Progress
    | Done value
