module Loaded.Data.Nonce.Lazy exposing (LazyLoadedDataNonce(..))

{-| `LazyLoadedDataNonce` models data which...

1.  Needs to be loaded
2.  Does not begin loading immediately but only when required
3.  May load incrementally
4.  Cannot fail to load
5.  Will _NOT_ reload


# Model

@docs LazyLoadedDataNonce

-}

import Loaded.Progress exposing (Progress)


{-| -}
type LazyLoadedDataNonce a
    = Initial
    | Pending Progress
    | Done a
