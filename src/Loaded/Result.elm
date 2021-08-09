module Loaded.Result exposing (LoadedResult(..))

{-|

@docs LoadedResult

-}

import Loaded.Progress exposing (Progress)


{-| Represents data which

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  May fail to load
5.  May _NOT_ reload

-}
type LoadedResult error value
    = Pending Progress
    | Failure error
    | Done value
