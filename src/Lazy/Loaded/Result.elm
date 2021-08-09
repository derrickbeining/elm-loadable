module Lazy.Loaded.Result exposing
    ( LazyLoadedResult(..)
    , singleton, fromMaybe, fromResult
    , fold, withDefault, extract, getError
    , toMaybe, toResult, toTask
    , isInitial, isPending, isFailure, isDone
    , alt, altLazy
    , andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList
    )

{-| `LazyLoadedResult` models data which...

1.  Needs to be loaded
2.  Does _NOT_ begin loading immediately and may never begin to load
3.  May load incrementally
4.  May fail to load
5.  May reload


# Model

@docs LazyLoadedResult


# Constructors

@docs singleton, fromMaybe, fromResult


# Destructors

@docs fold, withDefault, extract, getError


# Folds

@docs toMaybe, toResult, toTask


# Guards

@docs isInitial, isPending, isFailure, isDone


# Alternative

@docs alt, altLazy

@docs andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList

-}

import Array exposing (Array)
import Loaded.Progress exposing (Progress)
import Loaded.ReloadStatus exposing (ReloadStatus(..))
import Task exposing (Task)


{-| -}
type LazyLoadedResult error value
    = Initial
    | Pending Progress
    | Failure error
    | Done value



-- Constructors


{-| -}
singleton : a -> LazyLoadedResult e a
singleton a =
    Done a


{-| -}
fromMaybe : Maybe a -> LazyLoadedResult e a
fromMaybe ma =
    case ma of
        Nothing ->
            Initial

        Just val ->
            Done val


{-| -}
fromResult : Result e a -> LazyLoadedResult e a
fromResult result =
    case result of
        Err err ->
            Failure err

        Ok val ->
            Done val



-- Destructors


{-| -}
fold : (() -> val) -> (() -> val) -> (err -> val) -> (a -> val) -> LazyLoadedResult err a -> val
fold onInitial onPending onFailure onDone result =
    case result of
        Initial ->
            onInitial ()

        Pending _ ->
            onPending ()

        Failure err ->
            onFailure err

        Done val ->
            onDone val


{-| -}
withDefault : a -> LazyLoadedResult e a -> a
withDefault fallback fa =
    case fa of
        Done val ->
            val

        _ ->
            fallback


{-| -}
extract : (Maybe err -> a) -> LazyLoadedResult err a -> a
extract recover result =
    case result of
        Initial ->
            recover Nothing

        Pending _ ->
            recover Nothing

        Failure err ->
            recover <| Just err

        Done val ->
            val


{-| -}
getError : LazyLoadedResult err a -> Maybe err
getError result =
    case result of
        Failure err ->
            Just err

        _ ->
            Nothing



-- Predicates


{-| -}
isInitial : LazyLoadedResult e a -> Bool
isInitial data =
    case data of
        Initial ->
            True

        _ ->
            False


{-| -}
isPending : LazyLoadedResult e a -> Bool
isPending data =
    case data of
        Pending _ ->
            True

        _ ->
            False


{-| -}
isFailure : LazyLoadedResult e a -> Bool
isFailure data =
    case data of
        Failure _ ->
            True

        _ ->
            False


{-| -}
isDone : LazyLoadedResult e a -> Bool
isDone data =
    case data of
        Done _ ->
            True

        _ ->
            False



-- Transformation


{-| -}
map : (a -> b) -> LazyLoadedResult e a -> LazyLoadedResult e b
map f data =
    case data of
        Initial ->
            Initial

        Pending progress ->
            Pending progress

        Failure e ->
            Failure e

        Done a ->
            Done (f a)


{-| -}
mapError : (x -> y) -> LazyLoadedResult x a -> LazyLoadedResult y a
mapError f result =
    case result of
        Initial ->
            Initial

        Pending progress ->
            Pending progress

        Failure err ->
            Failure (f err)

        Done val ->
            Done val


{-| -}
bimap : (err -> e) -> (a -> b) -> LazyLoadedResult err a -> LazyLoadedResult e b
bimap onErr onDone result =
    result |> map onDone |> mapError onErr


{-| -}
flatten : LazyLoadedResult e (LazyLoadedResult e a) -> LazyLoadedResult e a
flatten data =
    case data of
        Initial ->
            Initial

        Pending progress ->
            Pending progress

        Failure err ->
            Failure err

        Done nested ->
            nested


{-| -}
join : LazyLoadedResult e (LazyLoadedResult e a) -> LazyLoadedResult e a
join =
    flatten


{-| -}
apply :
    LazyLoadedResult e a
    -> LazyLoadedResult e (a -> b)
    -> LazyLoadedResult e b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| -}
andMap : LazyLoadedResult e a -> LazyLoadedResult e (a -> b) -> LazyLoadedResult e b
andMap =
    apply


{-| -}
map2 :
    (a -> b -> c)
    -> LazyLoadedResult e a
    -> LazyLoadedResult e b
    -> LazyLoadedResult e c
map2 f fa ga =
    Done f
        |> apply fa
        |> apply ga


{-| -}
map3 :
    (a -> b -> c -> d)
    -> LazyLoadedResult e a
    -> LazyLoadedResult e b
    -> LazyLoadedResult e c
    -> LazyLoadedResult e d
map3 f fa ga ha =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> LazyLoadedResult err a
    -> LazyLoadedResult err b
    -> LazyLoadedResult err c
    -> LazyLoadedResult err d
    -> LazyLoadedResult err e
map4 f fa ga ha ia =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> LazyLoadedResult err a
    -> LazyLoadedResult err b
    -> LazyLoadedResult err c
    -> LazyLoadedResult err d
    -> LazyLoadedResult err e
    -> LazyLoadedResult err f
map5 f fa ga ha ia ja =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| -}
andThen :
    (a -> LazyLoadedResult e b)
    -> LazyLoadedResult e a
    -> LazyLoadedResult e b
andThen f fa =
    flatten <| map f fa


{-| -}
andThen2 :
    (a -> b -> LazyLoadedResult e c)
    -> LazyLoadedResult e a
    -> LazyLoadedResult e b
    -> LazyLoadedResult e c
andThen2 f fa fb =
    Done f
        |> apply fa
        |> apply fb
        |> flatten


{-| -}
andThen3 :
    (a -> b -> c -> LazyLoadedResult e d)
    -> LazyLoadedResult e a
    -> LazyLoadedResult e b
    -> LazyLoadedResult e c
    -> LazyLoadedResult e d
andThen3 f fa fb fc =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| -}
andThen4 :
    (a -> b -> c -> d -> LazyLoadedResult err e)
    -> LazyLoadedResult err a
    -> LazyLoadedResult err b
    -> LazyLoadedResult err c
    -> LazyLoadedResult err d
    -> LazyLoadedResult err e
andThen4 f fa fb fc fd =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> flatten


{-| -}
andThen5 :
    (a -> b -> c -> d -> e -> LazyLoadedResult err f)
    -> LazyLoadedResult err a
    -> LazyLoadedResult err b
    -> LazyLoadedResult err c
    -> LazyLoadedResult err d
    -> LazyLoadedResult err e
    -> LazyLoadedResult err f
andThen5 f fa fb fc fd fe =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| -}
toResult : Result e a -> LazyLoadedResult e a -> Result e a
toResult fallback result =
    case result of
        Initial ->
            fallback

        Pending _ ->
            fallback

        Failure err ->
            Err err

        Done val ->
            Ok val


{-| -}
toMaybe : LazyLoadedResult e a -> Maybe a
toMaybe result =
    case result of
        Done val ->
            Just val

        _ ->
            Nothing


{-| -}
toTask : Result err a -> LazyLoadedResult err a -> Task err a
toTask fallback result =
    case toResult fallback result of
        Err err ->
            Task.fail err

        Ok val ->
            Task.succeed val


{-| -}
filter : (a -> Bool) -> LazyLoadedResult e a -> LazyLoadedResult e a
filter test result =
    result
        |> andThen
            (\val ->
                if test val then
                    result

                else
                    Initial
            )


{-| -}
filterMap : (a -> LazyLoadedResult e (Maybe b)) -> LazyLoadedResult e a -> LazyLoadedResult e b
filterMap f result =
    result
        |> andThen f
        |> andThen fromMaybe


{-| -}
sequenceList : List (LazyLoadedResult e a) -> LazyLoadedResult e (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Done []) fas


{-| -}
traverseList : (a -> LazyLoadedResult err b) -> List (LazyLoadedResult err a) -> LazyLoadedResult err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| -}
sequenceArray : Array (LazyLoadedResult e a) -> LazyLoadedResult e (Array a)
sequenceArray fas =
    Array.foldl (map2 Array.push) (Done Array.empty) fas


{-| -}
traverseArray : (a -> LazyLoadedResult err b) -> Array (LazyLoadedResult err a) -> LazyLoadedResult err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray


{-| -}
alt : LazyLoadedResult err a -> LazyLoadedResult err a -> LazyLoadedResult err a
alt fallback result =
    altLazy (always fallback) result


{-| -}
altLazy : (() -> LazyLoadedResult err a) -> LazyLoadedResult err a -> LazyLoadedResult err a
altLazy getFallback result =
    if not (isDone result) then
        getFallback ()

    else
        result
