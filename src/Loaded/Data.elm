module Loaded.Data exposing (LoadedData(..))

{-| `LoadedData` models data which...

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  Cannot fail to load
5.  Will _NOT_ reload


# Model

@docs LoadedData

-}

import Loaded.Progress exposing (Progress)


{-| -}
type LoadedData value
    = Pending Progress
    | Done value
