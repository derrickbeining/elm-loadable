module Reloadable.Lazy exposing
    ( LazyReloadable
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

{-| This module provides only a type alias with no additional functions. See the
**[Loadable.Lazy](Loadable.Lazy)** module for available functions.


# Model

@docs LazyReloadable


# Natural Transformations

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
import Loadable.Lazy exposing (LazyLoadable(..))
import Set exposing (Set)
import Task exposing (Task)


{-| An alias of **[LazyLoadable](Loadable.Lazy#LazyLoadable)** configured to
represent reloadable data. It basically provides four (4) cases in
addition to what is provided by **[LazyLoadable](Loadable.Lazy)**. For more
info, see the docs for **[Reloadable](Reloadable#Reloadable)**, since they are
conceptually the same except for the underlying type.
-}
type alias LazyReloadable loading error value =
    LazyLoadable
        ( Maybe error, Maybe value, loading )
        ( error, Maybe value )
        value



-- Natural Transformations


{-| Transforms [LazyReloadable](#LazyReloadable) to a singleton `List` if it has a reference to
a loaded value, otherwise to an empty list.

    import Loadable.Lazy exposing (LazyLoadable(..))

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
toList : LazyReloadable loading error value -> List value
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


{-| Transforms [LazyReloadable](#LazyReloadable) to a singleton `Array` if it has a reference to
a loaded value, otherwise to an empty `Array`.

    import Array
    import Loadable.Lazy exposing (LazyLoadable(..))

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
toArray : LazyReloadable loading error value -> Array value
toArray data =
    Array.fromList <| toList data


{-| Transforms [LazyReloadable](#LazyReloadable) to a singleton `Set` if it has a reference to
a loaded value, otherwise to an empty \`Set.

    import Set
    import Loadable.Lazy exposing (LazyLoadable(..))

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
toSet : LazyReloadable loading error comparable -> Set comparable
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


{-| Transforms [LazyReloadable](#LazyReloadable) to a [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result),
taking a fallback **`Result`** in case of `Initial` or `Loading`..

    import Loadable.Lazy exposing (LazyLoadable(..))

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
toResult : (Maybe loading -> Result err value) -> LazyReloadable loading err value -> Result ( err, Maybe value ) value
toResult mkFallback loadable =
    case loadable of
        Initial ->
            mkFallback Nothing |> Result.mapError (\e -> ( e, Nothing ))

        Loading ( Just err, mVal, _ ) ->
            Result.Err ( err, mVal )

        Loading ( Nothing, Just val, _ ) ->
            Result.Ok val

        Loading ( Nothing, Nothing, loading ) ->
            mkFallback (Just loading) |> Result.mapError (\e -> ( e, Nothing ))

        Error err ->
            Result.Err err

        Loaded val ->
            Result.Ok val


{-| Transforms [LazyReloadable](#LazyReloadable) to [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe),
throwing away `loading` and/or `error` values.

    import Loadable.Lazy exposing (LazyLoadable(..))

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
toMaybe : LazyReloadable loading err value -> Maybe value
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


{-| Transforms [LazyReloadable](#LazyReloadable) to a [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task),
throwing away `loading` and taking a fallback [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result)
in case of `Initial` or `Loading`.

    import Task
    import Loadable.Lazy exposing (LazyLoadable(..))

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
    (Maybe loading -> Task err value)
    -> LazyReloadable loading err value
    -> Task ( err, Maybe value ) value
toTask mkFallback result =
    case result of
        Initial ->
            mkFallback Nothing |> Task.mapError (\e -> ( e, Nothing ))

        Loading ( Nothing, Nothing, loading ) ->
            mkFallback (Just loading) |> Task.mapError (\e -> ( e, Nothing ))

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

    import Loadable.Lazy exposing (LazyLoadable(..))

    Loading (Nothing, Just 1, ())
        |> map ((+) 1)
    --> Loading (Nothing, Just 2, ())

    Loaded 1
        |> map ((+) 1)
    --> Loaded 2

-}
map : (a -> b) -> LazyReloadable loading err a -> LazyReloadable loading err b
map f data =
    case data of
        Initial ->
            Initial

        Loading ( err0, val0, loading ) ->
            Loading ( err0, Maybe.map f val0, loading )

        Error ( err, lastVal ) ->
            Error ( err, Maybe.map f lastVal )

        Loaded a ->
            Loaded (f a)


{-| Apply a function to the **`Error`** value, otherwise a no-op.

    import Loadable.Lazy exposing (LazyLoadable(..))

    Loaded 1
        |> mapError String.toUpper
    --> Loaded 1

    Error ("Whoops!", Just 1)
        |> mapError String.toUpper
    --> Error ("WHOOPS!", Just 1)

-}
mapError :
    (errA -> errB)
    -> LazyReloadable loading errA a
    -> LazyReloadable loading errB a
mapError f result =
    case result of
        Initial ->
            Initial

        Loading ( err0, val, loading ) ->
            Loading ( Maybe.map f err0, val, loading )

        Error ( err, mVal ) ->
            Error ( f err, mVal )

        Loaded val ->
            Loaded val


{-| Apply one function to the **`Error`** value, or another function to the
**`Loaded`** value, otherwise a no-op.

    import Loadable.Lazy exposing (LazyLoadable(..))

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
    -> LazyReloadable loading errA a
    -> LazyReloadable loading errB b
mapBoth onErr onLoaded result =
    result |> map onLoaded |> mapError onErr


{-| Apply a function to the `Loading` value, otherwise a no-op.

    import Loadable.Lazy exposing (LazyLoadable(..))

    Loaded "Baked Potato"
        |> mapLoading (\loading -> {loading | progress = 0})
    --> Loaded "Baked Potato"

    Loading (Just "error", Just 1, {progress = 0})
        |> mapLoading (\loading -> {loading | progress = loading.progress + 1})
    --> Loading (Just "error", Just 1, {progress = 1})

-}
mapLoading :
    (loadingA -> loadingB)
    -> LazyReloadable loadingA err a
    -> LazyReloadable loadingB err a
mapLoading f result =
    case result of
        Initial ->
            Initial

        Loading ( e, a, loading ) ->
            Loading ( e, a, f loading )

        Error err ->
            Error err

        Loaded val ->
            Loaded val
