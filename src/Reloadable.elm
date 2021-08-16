module Reloadable exposing
    ( Reloadable
    , isRetrying, isReloading, isReloadError, isRetryingReload
    )

{-| This module provides only a type alias and a few additioanl additional functions. See the
**[Loadable](Loadable)** module for all other available functions.


# Model

@docs Reloadable


# Guards

@docs isRetrying, isReloading, isReloadError, isRetryingReload

-}

import Loadable exposing (Loadable(..))


{-| An alias of **[Loadable](Loadable#Loadable)** configured to represent
reloadable data. It basically provides four (4) cases in addition to what is
provided by **[Loadable](Loadable)**. In particular, it can represent the
following additional states:

1.  **Retrying**: Retrying after first **`Error`**, where you may not want to immediately throw away the original error.
2.  **Reloading**: Reloading after **`Loaded`**, where you may not want to throw away the already **`Loaded`** value.
3.  **Reload Error**: An **`Error`** after reloading, where you may not want to throw away the already **`Loaded`** value.
4.  **Retrying Reload**: Retrying after a reloading error, where you may not want to throw away the reload error or the already loaded value.

To visualize the algebra of **`Reloadable`**, the table below catalogs the meaning
of every possible pattern:

    | Meaning               | Pattern                                  |
    | --------------------- |----------------------------------------- |
    | Initial Loading       | Loading (Nothing, Nothing, loading)           |
    | Initial Load Error    | Error (err, Nothing)                     |
    | Retrying Initial Load | Loading (Just err, Nothing, loading)          |
    | Loaded                | Loaded value                             |
    | Reloading             | Loading (Nothing, Just val, loading)          |
    | Reload Error          | Error (err, Just lastLoadedVal)          |
    | Retrying Reload       | Loading (Just prevErr, Just prevVal, loading) |

Note that the **`loading`** type parameter is still available, allowing you to
tack on any additional metadata you'd like to track during the **`Loading`** state.

-}
type alias Reloadable loading err val =
    Loadable
        ( Maybe err, Maybe val, loading )
        ( err, Maybe val )
        val


{-| Predicates that the data is `Loading` after having previously
been `Loaded`.

    import Loadable exposing (Loadable(..))

    isReloading (Loading (Nothing, Just "prev loaded value", ()))
    --> True

    isReloading (Loading (Nothing, Nothing, ()))
    --> False

-}
isReloading : Reloadable loading error value -> Bool
isReloading data =
    case data of
        Loading ( _, Just _, _ ) ->
            True

        _ ->
            False


{-| Predicates that the data is `Loading` again after initial loading
`Error`(s), but hasn't ever been `Loaded` yet.

    import Loadable exposing (Loadable(..))

    isRetrying (Loading (Just "initial load error", Nothing, ()))
    --> True

    isRetrying (Loading (Nothing, Just "prev loaded value", ()))
    --> False

-}
isRetrying : Reloadable loading error value -> Bool
isRetrying data =
    case data of
        Loading ( Just _, Nothing, _ ) ->
            True

        _ ->
            False


{-| Predicates that the data is `Error` after having been `Loaded` and an
attemtped reload failed.

    import Loadable exposing (Loadable(..))

    isReloadError (Error ("Bad thing happened", Just "prev loaded value"))
    --> True

    isReloadError (Error ("Bad thing happened", Nothing))
    --> False

-}
isReloadError : Reloadable loading error value -> Bool
isReloadError data =
    case data of
        Error ( _, Just _ ) ->
            True

        _ ->
            False


{-| Predicates that the data is `Loading` again after having been `Loaded`
and then an `Error` happened during reload.

    import Loadable exposing (Loadable(..))

    isRetryingReload (Loading (Just "Bad things happened", Just "prev loaded value", ()))
    --> True

    isRetryingReload (Loading (Nothing, Just "prev loaded value", ()))
    --> False

-}
isRetryingReload : Reloadable loading error value -> Bool
isRetryingReload data =
    case data of
        Loading ( Just _, Just _, _ ) ->
            True

        _ ->
            False
