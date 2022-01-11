module Reloadable exposing
    ( Reloadable
    , loading, error, loaded
    , fromLoadable
    , isRetrying, isReloading, isReloadError, isRetryingReload
    , toLoadable
    , toMaybe
    , toResult
    , toList
    , toArray
    , toSet
    , toTask
    , map
    , mapBoth
    , mapError
    , mapLoading
    )

{-| This module provides only a type alias and a few additioanl additional functions. See the
**[Loadable](Loadable)** module for other available functions.


# Model

@docs Reloadable


# Constructors

@docs loading, error, loaded
@docs fromLoadable


# Guards

@docs isRetrying, isReloading, isReloadError, isRetryingReload


# Natural Transformations

@docs toLoadable
@docs toMaybe
@docs toResult
@docs toList
@docs toArray
@docs toSet
@docs toTask


# Combinators

@docs map
@docs mapBoth
@docs mapError
@docs mapLoading

-}

import Array exposing (Array)
import Loadable exposing (Loadable(..))
import Set exposing (Set)
import Task exposing (Task)


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


{-| -}
loading : Maybe err -> Maybe val -> loading -> Reloadable loading err val
loading mErr mVal loading_ =
    Loading ( mErr, mVal, loading_ )


{-| -}
error : err -> Maybe val -> Reloadable loading err val
error err mVal =
    Error ( err, mVal )


{-| -}
loaded : val -> Reloadable loading err val
loaded =
    Loaded


{-| Convert a `Reloadable` value to a `Loadable`
-}
toLoadable : Reloadable loading err val -> Loadable loading err val
toLoadable reloadable =
    case reloadable of
        Loading ( _, _, loading_ ) ->
            Loading loading_

        Error ( err, _ ) ->
            Error err

        Loaded val ->
            Loaded val


{-| Convert a `Loadable` value to a `Reloadable`
-}
fromLoadable : Loadable loading err val -> Reloadable loading err val
fromLoadable loadable =
    case loadable of
        Loading loading_ ->
            Loading ( Nothing, Nothing, loading_ )

        Error err ->
            Error ( err, Nothing )

        Loaded val ->
            Loaded val


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



-- Natural Transformations


{-| Transforms [Reloadable](#Reloadable) to a singleton `List` if it has a reference to
a loaded value, otherwise to an empty list.

    import Loadable exposing (Loadable(..))

    toList (Loading (Nothing, Nothing, ()))
    --> []

    toList (Loading (Nothing, Just 1, ()))
    --> [1]

    toList (Error ("oh noes", Just 1))
    --> [1]

    toList (Error ("oh noes", Nothing))
    --> []

    toList (Loaded "sup")
    ---> ["sup"])

-}
toList : Reloadable loading error value -> List value
toList data =
    case data of
        Loading ( _, Just val, _ ) ->
            [ val ]

        Error ( _, Just val ) ->
            [ val ]

        Loaded val ->
            [ val ]

        _ ->
            []


{-| Transforms [Reloadable](#Reloadable) to a singleton `Array` if it has a reference to
a loaded value, otherwise to an empty `Array`.

    import Array
    import Loadable exposing (Loadable(..))

    toArray (Loading (Nothing, Nothing, ()))
    --> Array.fromList []

    toArray (Loading (Nothing, Just 1, ()))
    --> Array.fromList [1]

    toArray (Error ("oh noes", Just 1))
    --> Array.fromList [1]

    toArray (Error ("oh noes", Nothing))
    --> Array.fromList []

    toArray (Loaded "sup")
    ---> Array.fromList ["sup"])

-}
toArray : Reloadable loading error value -> Array value
toArray data =
    Array.fromList <| toList data


{-| Transforms [Reloadable](#Reloadable) to a singleton `Set` if it has a reference to
a loaded value, otherwise to an empty \`Set.

    import Set
    import Loadable exposing (Loadable(..))

    toSet (Loading (Nothing, Nothing, ()))
    --> Set.fromList []

    toSet (Loading (Nothing, Just 1, ()))
    --> Set.fromList [1]

    toSet (Error ("oh noes", Just 1))
    --> Set.fromList [1]

    toSet (Error ("oh noes", Nothing))
    --> Set.fromList []

    toSet (Loaded "sup")
    ---> Set.fromList ["sup"])

-}
toSet : Reloadable loading error comparable -> Set comparable
toSet data =
    case data of
        Loading ( _, Just val, _ ) ->
            Set.fromList [ val ]

        Error ( _, Just val ) ->
            Set.fromList [ val ]

        Loaded val ->
            Set.fromList [ val ]

        _ ->
            Set.empty


{-| Transforms [Reloadable](#Reloadable) to a [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result),
taking a fallback **`Result`** in case of [Loading](#Loadable).

    import Loadable exposing (Loadable(..))

    Loading (Nothing, Nothing, ()) |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Err ("It was loading", Nothing)

    Loading (Nothing, Nothing, ()) |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "It was loading"

    Loading (Nothing, Just "previous", ()) |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Ok "previous"

    Loading (Just "error", Just "previous", ()) |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Err ("error", Just "previous")

    Loading (Just "error", Nothing, ()) |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Err ("error", Nothing)

    Error ("Oh no", Just "previous") |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Err ("Oh no", Just "previous")

    Loaded "All done" |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "All done"

-}
toResult : (loading -> Result err value) -> Reloadable loading err value -> Result ( err, Maybe value ) value
toResult onLoading loadable =
    case loadable of
        Loading ( Just err, mVal, _ ) ->
            Result.Err ( err, mVal )

        Loading ( Nothing, Just val, _ ) ->
            Result.Ok val

        Loading ( Nothing, Nothing, loading_ ) ->
            onLoading loading_ |> Result.mapError (\e -> ( e, Nothing ))

        Error err ->
            Result.Err err

        Loaded val ->
            Result.Ok val


{-| Transforms [Reloadable](#Reloadable) to [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe),
throwing away `loading` and/or `error` values.

    import Loadable exposing (Loadable(..))

    toMaybe (Loading (Nothing, Nothing, ()))
    --> Nothing

    toMaybe (Loading (Nothing, Just 1, ()))
    --> Just 1

    toMaybe (Error ("Oh no", Just 1))
    --> Just 1

    toMaybe (Error ("Oh no", Nothing))
    --> Nothing

    toMaybe (Loaded "All done")
    --> Just "All done"

-}
toMaybe : Reloadable loading err value -> Maybe value
toMaybe result =
    case result of
        Loading ( Just _, _, _ ) ->
            Nothing

        Loading ( Nothing, Just val, _ ) ->
            Just val

        Error ( _, Just val ) ->
            Just val

        Error ( _, Nothing ) ->
            Nothing

        Loaded val ->
            Just val

        _ ->
            Nothing


{-| Transforms [Reloadable](#Reloadable) to a [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task),
throwing away `loading` and taking a fallback [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result)
in case of [Loading](#Loadable).

    import Task
    import Loadable exposing (Loadable(..))

    Loading (Just "error", Nothing, ()) |> toTask (\_ -> Task.fail "It was loading")
    --> Task.fail ("error", Nothing)

    Loading (Just "error", Just "previous", ()) |> toTask (\_ -> Task.fail "It was loading")
    --> Task.fail ("error", Just "previous")

    Loading (Nothing, Just 1, ()) |> toTask (\_ -> Task.fail "It was loading")
    --> Task.succeed 1

    Error ("Oh no", Just "previous") |> toTask (\_ -> Task.succeed "It was loading")
    --> Task.fail ("Oh no", Just "previous")

    Loaded "All done" |> toTask (\_ -> Task.succeed "It was loading")
    --> Task.succeed "All done"

-}
toTask :
    (loading -> Task err value)
    -> Reloadable loading err value
    -> Task ( err, Maybe value ) value
toTask onLoading result =
    case result of
        Loading ( Nothing, Nothing, loading_ ) ->
            onLoading loading_ |> Task.mapError (\e -> ( e, Nothing ))

        Loading ( Just err, mVal, _ ) ->
            Task.fail ( err, mVal )

        Loading ( Nothing, Just val, _ ) ->
            Task.succeed val

        Error err ->
            Task.fail err

        Loaded val ->
            Task.succeed val



-- Combinators


{-| Apply a function to the **`Loaded`** value, otherwise a no-op.

    import Loadable exposing (Loadable(..))

    Loading (Nothing, Just 1, ())
        |> map ((+) 1)
    --> Loading (Nothing, Just 2, ())

    Loaded 1
        |> map ((+) 1)
    --> Loaded 2

-}
map : (a -> b) -> Reloadable loading err a -> Reloadable loading err b
map f data =
    case data of
        Loading ( err0, val0, loading_ ) ->
            Loading ( err0, Maybe.map f val0, loading_ )

        Error ( err, lastVal ) ->
            Error ( err, Maybe.map f lastVal )

        Loaded a ->
            Loaded (f a)


{-| Apply a function to the **`Error`** value, otherwise a no-op.

    import Loadable exposing (Loadable(..))

    Loaded 1
        |> mapError String.toUpper
    --> Loaded 1

    Error ("Whoops!", Just 1)
        |> mapError String.toUpper
    --> Error ("WHOOPS!", Just 1)

-}
mapError :
    (errA -> errB)
    -> Reloadable loading errA a
    -> Reloadable loading errB a
mapError f result =
    case result of
        Loading ( err0, val, loading_ ) ->
            Loading ( Maybe.map f err0, val, loading_ )

        Error ( err, mVal ) ->
            Error ( f err, mVal )

        Loaded val ->
            Loaded val


{-| Apply one function to the **`Error`** value, or another function to the
**`Loaded`** value, otherwise a no-op.

    import Loadable exposing (Loadable(..))

    Loading (Just "Error", Just 1, ())
        |> mapBoth String.toUpper ((+) 1)
    --> Loading (Just "ERROR", Just 2, () )

    Error ("Whoops!", Just 1)
        |> mapBoth String.toUpper ((+) 1)
    --> Error ("WHOOPS!", Just 2)

    Loaded 9000
        |> mapBoth String.toUpper ((+) 1)
    --> Loaded 9001

-}
mapBoth :
    (errA -> errB)
    -> (a -> b)
    -> Reloadable loading errA a
    -> Reloadable loading errB b
mapBoth onErr onLoaded result =
    result |> map onLoaded |> mapError onErr


{-| Apply a function to the `Loading` value, otherwise a no-op.

    import Loadable exposing (Loadable(..))

    Loaded "Baked Potato"
        |> mapLoading (\loading -> {loading | progress = 0})
    --> Loaded "Baked Potato"

    Loading (Just "error", Just 1, {progress = 0})
        |> mapLoading (\loading -> {loading | progress = loading.progress + 1})
    --> Loading (Just "error", Just 1, {progress = 1})

-}
mapLoading :
    (loadingA -> loadingB)
    -> Reloadable loadingA err a
    -> Reloadable loadingB err a
mapLoading f result =
    case result of
        Loading ( e, a, loading_ ) ->
            Loading ( e, a, f loading_ )

        Error err ->
            Error err

        Loaded val ->
            Loaded val
