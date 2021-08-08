module Loaded.Result.Nonce exposing (LoadedResultNonce(..))

{-|

@docs LoadedResultNonce

-}

import Loaded.Progress exposing (Progress)


{-| Represents data which

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  May fail to load
5.  May _NOT_ reload

-}
type LoadedResultNonce err a
    = Pending Progress
    | Failure err
    | Done a
