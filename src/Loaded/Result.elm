module Loaded.Result exposing
    ( LoadedResult(..)
    , singleton, fromMaybe, fromResult
    , fold, withDefault, getOrElse, getError
    , toMaybe, toResult, toTask
    , isPending, isFailure, isDone, isReloading, isInert
    , alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList
    )

{-| `LoadedResult` models data which...

1.  Needs to be loaded
2.  Begins loading immediately
3.  May load incrementally
4.  May fail to load
5.  May reload


# Model

@docs LoadedResult


# Constructors

@docs singleton, fromMaybe, fromResult


# Destructors

@docs fold, withDefault, getOrElse, getError


# Natural Transformations

@docs toMaybe, toResult, toTask


# Guards

@docs isPending, isFailure, isDone, isReloading, isInert


# Combinators

@docs alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList

-}

import Array exposing (Array)
import Loaded.Progress as Progress exposing (Progress)
import Loaded.ReloadStatus as ReloadStatus exposing (ReloadStatus(..))
import Platform exposing (Task)
import Task


{-| -}
type LoadedResult error value
    = Pending Progress
    | Failure ReloadStatus error
    | Done ReloadStatus value



-- Constructors


{-| Constructs an [Inert](Loaded.ReloadStatus#Inert) [Done](#Done)

    import Loaded.ReloadStatus as ReloadStatus

    singleton "hi"
    --> Done ReloadStatus.Inert "hi"

-}
singleton : a -> LoadedResult e a
singleton a =
    Done Inert a


{-|

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus as ReloadStatus

    fromMaybe Nothing
    --> Pending Progress.Untracked

    fromMaybe (Just "hi")
    --> Done ReloadStatus.Inert "hi"

-}
fromMaybe : Maybe a -> LoadedResult e a
fromMaybe ma =
    case ma of
        Nothing ->
            Pending Progress.Untracked

        Just val ->
            Done Inert val


{-|

    import Loaded.ReloadStatus as ReloadStatus

    fromResult (Err "oh noes!")
    --> Failure ReloadStatus.Inert "oh noes!"

    fromResult (Ok "Go")
    --> Done ReloadStatus.Inert "Go"

-}
fromResult : Result e a -> LoadedResult e a
fromResult result =
    case result of
        Err err ->
            Failure Inert err

        Ok val ->
            Done Inert val



-- Destructors


{-| Pattern matching, basically.

    import Loaded.ReloadStatus as ReloadStatus exposing (ReloadStatus)
    import Loaded.Progress as Progress exposing (Progress)

    onPending : Progress -> String
    onPending _ =
        "It was Pending"

    onFailure : ReloadStatus -> String -> String
    onFailure _ err =
        "Error! " ++ err

    onDone : ReloadStatus -> Int -> String
    onDone _ val =
        "It's over " ++ String.fromInt val ++ "!"

    Pending Progress.Untracked
        |> fold onPending onFailure onDone
    --> "It was Pending"

    Failure ReloadStatus.Inert "undefined is not a function"
        |> fold onPending onFailure onDone
    --> "Error! undefined is not a function"

    Done ReloadStatus.Inert 9000
        |> fold onPending onFailure onDone
    --> "It's over 9000!"

-}
fold :
    (Progress -> val)
    -> (ReloadStatus -> err -> val)
    -> (ReloadStatus -> a -> val)
    -> LoadedResult err a
    -> val
fold onPending onFailure onDone result =
    case result of
        Pending progress ->
            onPending progress

        Failure status err ->
            onFailure status err

        Done status val ->
            onDone status val


{-| Attempt to access the loaded value, falling back to the default value
provided if not `Done`.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus as ReloadStatus

    Pending Progress.Untracked
        |> withDefault "Potato"
    --> "Potato"

    Done ReloadStatus.Inert "Banana"
        |> withDefault "Potato"
    --> "Banana"

-}
withDefault : a -> LoadedResult e a -> a
withDefault fallback fa =
    case fa of
        Done _ val ->
            val

        _ ->
            fallback


{-| Attempt to access the loaded value, constructing a fallback value if the
data is not yet `Done`.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus as ReloadStatus

    Pending Progress.Untracked
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "No error!"

    Failure ReloadStatus.Inert "Something went wrong :("
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "Something went wrong :("

    Done ReloadStatus.Inert "All done"
        |> getOrElse (\mErr -> Maybe.withDefault "No error!" mErr)
    --> "All done"

-}
getOrElse : (Maybe err -> a) -> LoadedResult err a -> a
getOrElse mkFallback result =
    case result of
        Pending _ ->
            mkFallback Nothing

        Failure _ err ->
            mkFallback <| Just err

        Done _ val ->
            val


{-| Attempt to access the error in the Failure case.

    import Loaded.Progress as Progress
    import Loaded.ReloadStatus as ReloadStatus

    getError (Failure ReloadStatus.Inert "Wah wah wahhh")
    --> Just "Wah wah wahhh"

    getError (Pending Progress.Untracked)
    --> Nothing

-}
getError : LoadedResult err a -> Maybe err
getError result =
    case result of
        Failure _ err ->
            Just err

        _ ->
            Nothing



-- Guards


{-| -}
isPending : LoadedResult e a -> Bool
isPending data =
    case data of
        Pending _ ->
            True

        _ ->
            False


{-| -}
isFailure : LoadedResult e a -> Bool
isFailure data =
    case data of
        Failure _ _ ->
            True

        _ ->
            False


{-| -}
isDone : LoadedResult e a -> Bool
isDone data =
    case data of
        Done _ _ ->
            True

        _ ->
            False


{-| -}
isReloading : LoadedResult e a -> Bool
isReloading result =
    case result of
        Failure status _ ->
            ReloadStatus.isReloading status

        Done status _ ->
            ReloadStatus.isReloading status

        _ ->
            False


{-| -}
isInert : LoadedResult error a -> Bool
isInert =
    not << isReloading



-- Natural Transformations


{-| -}
toResult : Result e a -> LoadedResult e a -> Result e a
toResult fallback result =
    case result of
        Pending _ ->
            fallback

        Failure _ err ->
            Err err

        Done _ val ->
            Ok val


{-| -}
toMaybe : LoadedResult e a -> Maybe a
toMaybe result =
    case result of
        Done _ val ->
            Just val

        _ ->
            Nothing


{-| -}
toTask : Result err a -> LoadedResult err a -> Task err a
toTask fallback result =
    case toResult fallback result of
        Err err ->
            Task.fail err

        Ok val ->
            Task.succeed val



-- Combinators


{-| You know the drill.
-}
map : (a -> b) -> LoadedResult e a -> LoadedResult e b
map f data =
    case data of
        Pending progress ->
            Pending progress

        Failure status e ->
            Failure status e

        Done status a ->
            Done status (f a)


{-| -}
mapError : (x -> y) -> LoadedResult x a -> LoadedResult y a
mapError f result =
    case result of
        Pending progress ->
            Pending progress

        Failure status err ->
            Failure status (f err)

        Done status val ->
            Done status val


{-| -}
bimap : (err -> e) -> (a -> b) -> LoadedResult err a -> LoadedResult e b
bimap onErr onDone result =
    result |> map onDone |> mapError onErr


{-| -}
flatten : LoadedResult e (LoadedResult e a) -> LoadedResult e a
flatten data =
    case data of
        Pending progress ->
            Pending progress

        Failure status err ->
            Failure status err

        Done status (Done nestedStatus a) ->
            if List.any ReloadStatus.isReloading [ status, nestedStatus ] then
                Done (Reloading Progress.Untracked) a

            else
                Done Inert a

        Done _ nested ->
            nested


{-| -}
join : LoadedResult e (LoadedResult e a) -> LoadedResult e a
join =
    flatten


{-| -}
apply :
    LoadedResult e a
    -> LoadedResult e (a -> b)
    -> LoadedResult e b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| -}
andMap : LoadedResult e a -> LoadedResult e (a -> b) -> LoadedResult e b
andMap =
    apply


{-| -}
map2 :
    (a -> b -> c)
    -> LoadedResult e a
    -> LoadedResult e b
    -> LoadedResult e c
map2 f fa ga =
    Done Inert f
        |> apply fa
        |> apply ga


{-| -}
map3 :
    (a -> b -> c -> d)
    -> LoadedResult e a
    -> LoadedResult e b
    -> LoadedResult e c
    -> LoadedResult e d
map3 f fa ga ha =
    Done Inert f
        |> apply fa
        |> apply ga
        |> apply ha


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> LoadedResult err a
    -> LoadedResult err b
    -> LoadedResult err c
    -> LoadedResult err d
    -> LoadedResult err e
map4 f fa ga ha ia =
    Done Inert f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> LoadedResult err a
    -> LoadedResult err b
    -> LoadedResult err c
    -> LoadedResult err d
    -> LoadedResult err e
    -> LoadedResult err f
map5 f fa ga ha ia ja =
    Done Inert f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| -}
andThen :
    (a -> LoadedResult e b)
    -> LoadedResult e a
    -> LoadedResult e b
andThen f fa =
    flatten <| map f fa


{-| -}
andThen2 :
    (a -> b -> LoadedResult e c)
    -> LoadedResult e a
    -> LoadedResult e b
    -> LoadedResult e c
andThen2 f fa fb =
    Done Inert f
        |> apply fa
        |> apply fb
        |> flatten


{-| -}
andThen3 :
    (a -> b -> c -> LoadedResult e d)
    -> LoadedResult e a
    -> LoadedResult e b
    -> LoadedResult e c
    -> LoadedResult e d
andThen3 f fa fb fc =
    Done Inert f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| -}
andThen4 :
    (a -> b -> c -> d -> LoadedResult err e)
    -> LoadedResult err a
    -> LoadedResult err b
    -> LoadedResult err c
    -> LoadedResult err d
    -> LoadedResult err e
andThen4 f fa fb fc fd =
    Done Inert f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> flatten


{-| -}
andThen5 :
    (a -> b -> c -> d -> e -> LoadedResult err f)
    -> LoadedResult err a
    -> LoadedResult err b
    -> LoadedResult err c
    -> LoadedResult err d
    -> LoadedResult err e
    -> LoadedResult err f
andThen5 f fa fb fc fd fe =
    Done Inert f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| -}
alt : LoadedResult err a -> LoadedResult err a -> LoadedResult err a
alt fallback result =
    altLazy (always fallback) result


{-| -}
altLazy : (() -> LoadedResult err a) -> LoadedResult err a -> LoadedResult err a
altLazy getFallback result =
    if not (isDone result) then
        getFallback ()

    else
        result


{-| -}
filter : (a -> Bool) -> LoadedResult e a -> LoadedResult e a
filter test result =
    result
        |> andThen
            (\val ->
                if test val then
                    result

                else
                    Pending Progress.Untracked
            )


{-| -}
filterMap : (a -> LoadedResult e (Maybe b)) -> LoadedResult e a -> LoadedResult e b
filterMap f result =
    result
        |> andThen f
        |> andThen fromMaybe


{-| -}
sequenceList : List (LoadedResult e a) -> LoadedResult e (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Done Inert []) fas


{-| -}
traverseList : (a -> LoadedResult err b) -> List (LoadedResult err a) -> LoadedResult err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| -}
sequenceArray : Array (LoadedResult e a) -> LoadedResult e (Array a)
sequenceArray fas =
    Array.foldl (map2 Array.push) (Done Inert Array.empty) fas


{-| -}
traverseArray : (a -> LoadedResult err b) -> Array (LoadedResult err a) -> LoadedResult err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray
