module Lazy.Reloadable.Result exposing
    ( LazyReloadableResult(..)
    , singleton, fromMaybe, fromResult
    , fold, withDefault, getOrElse, getError
    , toMaybe, toResult, toTask
    , isLoading, isErr, isOk, isReloading
    , alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList
    )

{-| `LazyReloadableResult` models data which...

1.  Needs to be loaded
2.  Does NOT begin loading immediately and may never begin to load
3.  May load incrementally
4.  May fail to load
5.  May reload


# Model

@docs LazyReloadableResult


# Constructors

@docs singleton, fromMaybe, fromResult


# Destructors

@docs fold, withDefault, getOrElse, getError


# Natural Transformations

@docs toMaybe, toResult, toTask


# Guards

@docs isLoading, isErr, isOk, isReloading


# Combinators

@docs alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList

-}

import Array exposing (Array)
import Loaded.Progress as Progress exposing (Progress)
import Loaded.ReloadStatus as ReloadStatus exposing (ReloadStatus(..))
import Platform exposing (Task)
import Task


{-| -}
type LazyReloadableResult error value
    = Loading Progress
    | Err ReloadStatus error
    | Ok ReloadStatus value



-- Constructors


{-| Constructs an [Ok](#Ok) [NotReloading](Loaded.ReloadStatus#NotReloading) `value`

    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    singleton "hi"
    --> Lazy.Reloadable.Result.Ok NotReloading "hi"

-}
singleton : a -> LazyReloadableResult e a
singleton a =
    Ok NotReloading a


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    fromMaybe Nothing
    --> Lazy.Reloadable.Result.Loading Progress.Untracked

    fromMaybe (Just "hi")
    --> Lazy.Reloadable.Result.Ok NotReloading "hi"

-}
fromMaybe : Maybe a -> LazyReloadableResult e a
fromMaybe ma =
    case ma of
        Nothing ->
            Loading Progress.Untracked

        Just val ->
            Ok NotReloading val


{-|

    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    fromResult (Result.Err "oh noes!")
    --> Lazy.Reloadable.Result.Err NotReloading "oh noes!"

    fromResult (Result.Ok "Go")
    --> Lazy.Reloadable.Result.Ok NotReloading "Go"

-}
fromResult : Result e a -> LazyReloadableResult e a
fromResult result =
    case result of
        Result.Err err ->
            Err NotReloading err

        Result.Ok val ->
            Ok NotReloading val



-- Destructors


{-| Pattern matching, basically.

    import Loaded.ReloadStatus exposing (ReloadStatus(..))
    import Loaded.Progress as Progress exposing (Progress)

    onPending : Progress -> String
    onPending _ =
        "It was Loading"

    onFailure : ReloadStatus -> String -> String
    onFailure _ err =
        "Error! " ++ err

    onDone : ReloadStatus -> Int -> String
    onDone _ val =
        "It's over " ++ String.fromInt val ++ "!"

    Loading Progress.Untracked
        |> fold onPending onFailure onDone
    --> "It was Loading"

    Lazy.Reloadable.Result.Err NotReloading "undefined is not a function"
        |> fold onPending onFailure onDone
    --> "Error! undefined is not a function"

    Lazy.Reloadable.Result.Ok NotReloading 9000
        |> fold onPending onFailure onDone
    --> "It's over 9000!"

-}
fold :
    (Progress -> val)
    -> (ReloadStatus -> err -> val)
    -> (ReloadStatus -> a -> val)
    -> LazyReloadableResult err a
    -> val
fold onPending onFailure onDone result =
    case result of
        Loading progress ->
            onPending progress

        Err status err ->
            onFailure status err

        Ok status val ->
            onDone status val


{-| Attempt to access the loaded value, falling back to the default value
provided if not `Loaded`.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    Lazy.Reloadable.Result.Loading Progress.Untracked
        |> withDefault "Potato"
    --> "Potato"

    Lazy.Reloadable.Result.Ok NotReloading "Banana"
        |> withDefault "Potato"
    --> "Banana"

-}
withDefault : a -> LazyReloadableResult e a -> a
withDefault fallback fa =
    case fa of
        Ok _ val ->
            val

        _ ->
            fallback


{-| Attempt to access the loaded value, constructing a fallback value if the
data is not yet `Loaded`.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    Lazy.Reloadable.Result.Loading Progress.Untracked
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "No error!"

    Lazy.Reloadable.Result.Err NotReloading "Something went wrong :("
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "Something went wrong :("

    Lazy.Reloadable.Result.Ok NotReloading "All done"
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "All done"

-}
getOrElse : (Maybe err -> a) -> LazyReloadableResult err a -> a
getOrElse mkFallback result =
    case result of
        Loading _ ->
            mkFallback Nothing

        Err _ err ->
            mkFallback <| Just err

        Ok _ val ->
            val


{-| Attempt to access the error in the Err case.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    getError (Lazy.Reloadable.Result.Err NotReloading "Wah wah wahhh")
    --> Just "Wah wah wahhh"

    getError (Loading Progress.Untracked)
    --> Nothing

-}
getError : LazyReloadableResult err a -> Maybe err
getError result =
    case result of
        Err _ err ->
            Just err

        _ ->
            Nothing



-- Guards


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    isLoading (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> True

    isLoading (Lazy.Reloadable.Result.Ok NotReloading "hi")
    --> False

-}
isLoading : LazyReloadableResult e a -> Bool
isLoading data =
    case data of
        Loading _ ->
            True

        _ ->
            False


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    isErr (Loading Progress.Untracked)
    --> False

    isErr (Lazy.Reloadable.Result.Err NotReloading "Uh oh")
    --> True

-}
isErr : LazyReloadableResult e a -> Bool
isErr data =
    case data of
        Err _ _ ->
            True

        _ ->
            False


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    isOk (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> False

    isOk (Lazy.Reloadable.Result.Ok NotReloading "All done")
    --> True

-}
isOk : LazyReloadableResult e a -> Bool
isOk data =
    case data of
        Ok _ _ ->
            True

        _ ->
            False


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    isReloading (Loading Progress.Untracked)
    --> False

    isReloading (Lazy.Reloadable.Result.Err NotReloading "Uh oh")
    --> False

    isReloading (Lazy.Reloadable.Result.Err (Reloading Progress.Untracked) "Uh oh")
    --> True

-}
isReloading : LazyReloadableResult e a -> Bool
isReloading result =
    case result of
        Err status _ ->
            ReloadStatus.isReloading status

        Ok status _ ->
            ReloadStatus.isReloading status

        _ ->
            False



-- Natural Transformations


{-| Transforms [LazyReloadableResult](#LazyReloadableResult) to a [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result),
throwing away [ReloadStatus](Loaded.ReloadStatus) and taking a
fallback[Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result)
in case of [Loading](#LazyReloadableResult).

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    toResult (Result.Err "It was pending") (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> Result.Err "It was pending"

    toResult (Result.Ok "It was pending") (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> Result.Ok "It was pending"

    toResult (Result.Ok "It was pending") (Lazy.Reloadable.Result.Err NotReloading "Oh no")
    --> Result.Err "Oh no"

    toResult (Result.Ok "It was pending") (Lazy.Reloadable.Result.Ok NotReloading "All done")
    --> Result.Ok "All done"

-}
toResult : Result e a -> LazyReloadableResult e a -> Result e a
toResult fallback result =
    case result of
        Loading _ ->
            fallback

        Err _ err ->
            Result.Err err

        Ok _ val ->
            Result.Ok val


{-| Transforms [LazyReloadableResult](#LazyReloadableResult) to [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe),
throwing away [ReloadStatus](Loaded.ReloadStatus) and/or
[Err](#LazyReloadableResult) error info.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    toMaybe (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> Nothing

    toMaybe (Lazy.Reloadable.Result.Err NotReloading "Oh no")
    --> Nothing

    toMaybe (Lazy.Reloadable.Result.Ok NotReloading "All done")
    --> Just "All done"

-}
toMaybe : LazyReloadableResult e a -> Maybe a
toMaybe result =
    case result of
        Ok _ val ->
            Just val

        _ ->
            Nothing


{-| Transforms [LazyReloadableResult](#LazyReloadableResult) to a [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task),
throwing away [ReloadStatus](Loaded.ReloadStatus) and taking a
fallback[Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result)
in case of [Loading](#LazyReloadableResult).

    import Task
    import Loaded.Progress as Progress
    import Loaded.ReloadStatus exposing (ReloadStatus(..))

    toTask (Result.Ok "It was pending") (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> Task.succeed "It was pending"

    toTask (Result.Err "It was pending") (Lazy.Reloadable.Result.Loading Progress.Untracked)
    --> Task.fail "It was pending"

    toTask (Result.Ok "It was pending") (Lazy.Reloadable.Result.Err NotReloading "Oh no")
    --> Task.fail "Oh no"

    toTask (Result.Ok "It was pending") (Lazy.Reloadable.Result.Ok NotReloading "All done")
    --> Task.succeed "All done"

-}
toTask : Result err a -> LazyReloadableResult err a -> Task err a
toTask fallback result =
    case toResult fallback result of
        Result.Err err ->
            Task.fail err

        Result.Ok val ->
            Task.succeed val



-- Combinators


{-| Applies a function to the `Loaded` value if it is `Loaded`, otherwise
-}
map : (a -> b) -> LazyReloadableResult e a -> LazyReloadableResult e b
map f data =
    case data of
        Loading progress ->
            Loading progress

        Err status e ->
            Err status e

        Ok status a ->
            Ok status (f a)


{-| -}
mapError : (x -> y) -> LazyReloadableResult x a -> LazyReloadableResult y a
mapError f result =
    case result of
        Loading progress ->
            Loading progress

        Err status err ->
            Err status (f err)

        Ok status val ->
            Ok status val


{-| -}
bimap : (err -> e) -> (a -> b) -> LazyReloadableResult err a -> LazyReloadableResult e b
bimap onErr onDone result =
    result |> map onDone |> mapError onErr


{-| -}
flatten : LazyReloadableResult e (LazyReloadableResult e a) -> LazyReloadableResult e a
flatten data =
    case data of
        Loading progress ->
            Loading progress

        Err status err ->
            Err status err

        Ok status (Ok nestedStatus a) ->
            if List.any ReloadStatus.isReloading [ status, nestedStatus ] then
                Ok (Reloading Progress.Untracked) a

            else
                Ok NotReloading a

        Ok _ nested ->
            nested


{-| -}
join : LazyReloadableResult e (LazyReloadableResult e a) -> LazyReloadableResult e a
join =
    flatten


{-| -}
apply :
    LazyReloadableResult e a
    -> LazyReloadableResult e (a -> b)
    -> LazyReloadableResult e b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| -}
andMap : LazyReloadableResult e a -> LazyReloadableResult e (a -> b) -> LazyReloadableResult e b
andMap =
    apply


{-| -}
map2 :
    (a -> b -> c)
    -> LazyReloadableResult e a
    -> LazyReloadableResult e b
    -> LazyReloadableResult e c
map2 f fa ga =
    Ok NotReloading f
        |> apply fa
        |> apply ga


{-| -}
map3 :
    (a -> b -> c -> d)
    -> LazyReloadableResult e a
    -> LazyReloadableResult e b
    -> LazyReloadableResult e c
    -> LazyReloadableResult e d
map3 f fa ga ha =
    Ok NotReloading f
        |> apply fa
        |> apply ga
        |> apply ha


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> LazyReloadableResult err a
    -> LazyReloadableResult err b
    -> LazyReloadableResult err c
    -> LazyReloadableResult err d
    -> LazyReloadableResult err e
map4 f fa ga ha ia =
    Ok NotReloading f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> LazyReloadableResult err a
    -> LazyReloadableResult err b
    -> LazyReloadableResult err c
    -> LazyReloadableResult err d
    -> LazyReloadableResult err e
    -> LazyReloadableResult err f
map5 f fa ga ha ia ja =
    Ok NotReloading f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| -}
andThen :
    (a -> LazyReloadableResult e b)
    -> LazyReloadableResult e a
    -> LazyReloadableResult e b
andThen f fa =
    flatten <| map f fa


{-| -}
andThen2 :
    (a -> b -> LazyReloadableResult e c)
    -> LazyReloadableResult e a
    -> LazyReloadableResult e b
    -> LazyReloadableResult e c
andThen2 f fa fb =
    Ok NotReloading f
        |> apply fa
        |> apply fb
        |> flatten


{-| -}
andThen3 :
    (a -> b -> c -> LazyReloadableResult e d)
    -> LazyReloadableResult e a
    -> LazyReloadableResult e b
    -> LazyReloadableResult e c
    -> LazyReloadableResult e d
andThen3 f fa fb fc =
    Ok NotReloading f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| -}
andThen4 :
    (a -> b -> c -> d -> LazyReloadableResult err e)
    -> LazyReloadableResult err a
    -> LazyReloadableResult err b
    -> LazyReloadableResult err c
    -> LazyReloadableResult err d
    -> LazyReloadableResult err e
andThen4 f fa fb fc fd =
    Ok NotReloading f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> flatten


{-| -}
andThen5 :
    (a -> b -> c -> d -> e -> LazyReloadableResult err f)
    -> LazyReloadableResult err a
    -> LazyReloadableResult err b
    -> LazyReloadableResult err c
    -> LazyReloadableResult err d
    -> LazyReloadableResult err e
    -> LazyReloadableResult err f
andThen5 f fa fb fc fd fe =
    Ok NotReloading f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| -}
alt : LazyReloadableResult err a -> LazyReloadableResult err a -> LazyReloadableResult err a
alt fallback result =
    altLazy (always fallback) result


{-| -}
altLazy : (() -> LazyReloadableResult err a) -> LazyReloadableResult err a -> LazyReloadableResult err a
altLazy getFallback result =
    if not (isOk result) then
        getFallback ()

    else
        result


{-| -}
filter : (a -> Bool) -> LazyReloadableResult e a -> LazyReloadableResult e a
filter test result =
    result
        |> andThen
            (\val ->
                if test val then
                    result

                else
                    Loading Progress.Untracked
            )


{-| -}
filterMap : (a -> LazyReloadableResult e (Maybe b)) -> LazyReloadableResult e a -> LazyReloadableResult e b
filterMap f result =
    result
        |> andThen f
        |> andThen fromMaybe


{-| -}
sequenceList : List (LazyReloadableResult e a) -> LazyReloadableResult e (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Ok NotReloading []) fas


{-| -}
traverseList : (a -> LazyReloadableResult err b) -> List (LazyReloadableResult err a) -> LazyReloadableResult err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| -}
sequenceArray : Array (LazyReloadableResult e a) -> LazyReloadableResult e (Array a)
sequenceArray fas =
    Array.foldl (map2 Array.push) (Ok NotReloading Array.empty) fas


{-| -}
traverseArray : (a -> LazyReloadableResult err b) -> Array (LazyReloadableResult err a) -> LazyReloadableResult err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray
