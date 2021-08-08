module Loaded.Result.Nonce.Lazy exposing
    ( LazyLoadedResultNonce(..)
    , singleton, fromMaybe, fromResult
    , fold, withDefault, extract, getError
    , toMaybe, toResult, toTask
    , isInitial, isPending, isFailure, isDone
    , alt, altLazy
    , andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, filter, filterMap, flatten, join, map, map2, map3, map4, map5, mapError, sequenceArray, sequenceList, traverseArray, traverseList
    )

{-| LazyLoadedResultNonce models data which

1.  Needs to be loaded
2.  Does _NOT_ begin loading immediately
3.  May load incrementally
4.  May fail to load
5.  May reload


# Model

@docs LazyLoadedResultNonce


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
type LazyLoadedResultNonce e a
    = Initial
    | Pending Progress
    | Failure e
    | Done a



-- Constructors


{-| -}
singleton : a -> LazyLoadedResultNonce e a
singleton a =
    Done a


{-| -}
fromMaybe : Maybe a -> LazyLoadedResultNonce e a
fromMaybe ma =
    case ma of
        Nothing ->
            Initial

        Just val ->
            Done val


{-| -}
fromResult : Result e a -> LazyLoadedResultNonce e a
fromResult result =
    case result of
        Err err ->
            Failure err

        Ok val ->
            Done val



-- Destructors


{-| -}
fold : (() -> val) -> (() -> val) -> (err -> val) -> (a -> val) -> LazyLoadedResultNonce err a -> val
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
withDefault : a -> LazyLoadedResultNonce e a -> a
withDefault fallback fa =
    case fa of
        Done val ->
            val

        _ ->
            fallback


{-| -}
extract : (Maybe err -> a) -> LazyLoadedResultNonce err a -> a
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
getError : LazyLoadedResultNonce err a -> Maybe err
getError result =
    case result of
        Failure err ->
            Just err

        _ ->
            Nothing



-- Predicates


{-| -}
isInitial : LazyLoadedResultNonce e a -> Bool
isInitial data =
    case data of
        Initial ->
            True

        _ ->
            False


{-| -}
isPending : LazyLoadedResultNonce e a -> Bool
isPending data =
    case data of
        Pending _ ->
            True

        _ ->
            False


{-| -}
isFailure : LazyLoadedResultNonce e a -> Bool
isFailure data =
    case data of
        Failure _ ->
            True

        _ ->
            False


{-| -}
isDone : LazyLoadedResultNonce e a -> Bool
isDone data =
    case data of
        Done _ ->
            True

        _ ->
            False



-- Transformation


{-| -}
map : (a -> b) -> LazyLoadedResultNonce e a -> LazyLoadedResultNonce e b
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
mapError : (x -> y) -> LazyLoadedResultNonce x a -> LazyLoadedResultNonce y a
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
bimap : (err -> e) -> (a -> b) -> LazyLoadedResultNonce err a -> LazyLoadedResultNonce e b
bimap onErr onDone result =
    result |> map onDone |> mapError onErr


{-| -}
flatten : LazyLoadedResultNonce e (LazyLoadedResultNonce e a) -> LazyLoadedResultNonce e a
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
join : LazyLoadedResultNonce e (LazyLoadedResultNonce e a) -> LazyLoadedResultNonce e a
join =
    flatten


{-| -}
apply :
    LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e (a -> b)
    -> LazyLoadedResultNonce e b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| -}
andMap : LazyLoadedResultNonce e a -> LazyLoadedResultNonce e (a -> b) -> LazyLoadedResultNonce e b
andMap =
    apply


{-| -}
map2 :
    (a -> b -> c)
    -> LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e b
    -> LazyLoadedResultNonce e c
map2 f fa ga =
    Done f
        |> apply fa
        |> apply ga


{-| -}
map3 :
    (a -> b -> c -> d)
    -> LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e b
    -> LazyLoadedResultNonce e c
    -> LazyLoadedResultNonce e d
map3 f fa ga ha =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha


{-| -}
map4 :
    (a -> b -> c -> d -> e)
    -> LazyLoadedResultNonce err a
    -> LazyLoadedResultNonce err b
    -> LazyLoadedResultNonce err c
    -> LazyLoadedResultNonce err d
    -> LazyLoadedResultNonce err e
map4 f fa ga ha ia =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia


{-| -}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> LazyLoadedResultNonce err a
    -> LazyLoadedResultNonce err b
    -> LazyLoadedResultNonce err c
    -> LazyLoadedResultNonce err d
    -> LazyLoadedResultNonce err e
    -> LazyLoadedResultNonce err f
map5 f fa ga ha ia ja =
    Done f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| -}
andThen :
    (a -> LazyLoadedResultNonce e b)
    -> LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e b
andThen f fa =
    flatten <| map f fa


{-| -}
andThen2 :
    (a -> b -> LazyLoadedResultNonce e c)
    -> LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e b
    -> LazyLoadedResultNonce e c
andThen2 f fa fb =
    Done f
        |> apply fa
        |> apply fb
        |> flatten


{-| -}
andThen3 :
    (a -> b -> c -> LazyLoadedResultNonce e d)
    -> LazyLoadedResultNonce e a
    -> LazyLoadedResultNonce e b
    -> LazyLoadedResultNonce e c
    -> LazyLoadedResultNonce e d
andThen3 f fa fb fc =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| -}
andThen4 :
    (a -> b -> c -> d -> LazyLoadedResultNonce err e)
    -> LazyLoadedResultNonce err a
    -> LazyLoadedResultNonce err b
    -> LazyLoadedResultNonce err c
    -> LazyLoadedResultNonce err d
    -> LazyLoadedResultNonce err e
andThen4 f fa fb fc fd =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> flatten


{-| -}
andThen5 :
    (a -> b -> c -> d -> e -> LazyLoadedResultNonce err f)
    -> LazyLoadedResultNonce err a
    -> LazyLoadedResultNonce err b
    -> LazyLoadedResultNonce err c
    -> LazyLoadedResultNonce err d
    -> LazyLoadedResultNonce err e
    -> LazyLoadedResultNonce err f
andThen5 f fa fb fc fd fe =
    Done f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| -}
toResult : Result e a -> LazyLoadedResultNonce e a -> Result e a
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
toMaybe : LazyLoadedResultNonce e a -> Maybe a
toMaybe result =
    case result of
        Done val ->
            Just val

        _ ->
            Nothing


{-| -}
toTask : Result err a -> LazyLoadedResultNonce err a -> Task err a
toTask fallback result =
    case toResult fallback result of
        Err err ->
            Task.fail err

        Ok val ->
            Task.succeed val


{-| -}
filter : (a -> Bool) -> LazyLoadedResultNonce e a -> LazyLoadedResultNonce e a
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
filterMap : (a -> LazyLoadedResultNonce e (Maybe b)) -> LazyLoadedResultNonce e a -> LazyLoadedResultNonce e b
filterMap f result =
    result
        |> andThen f
        |> andThen fromMaybe


{-| -}
sequenceList : List (LazyLoadedResultNonce e a) -> LazyLoadedResultNonce e (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Done []) fas


{-| -}
traverseList : (a -> LazyLoadedResultNonce err b) -> List (LazyLoadedResultNonce err a) -> LazyLoadedResultNonce err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| -}
sequenceArray : Array (LazyLoadedResultNonce e a) -> LazyLoadedResultNonce e (Array a)
sequenceArray fas =
    Array.foldl (map2 Array.push) (Done Array.empty) fas


{-| -}
traverseArray : (a -> LazyLoadedResultNonce err b) -> Array (LazyLoadedResultNonce err a) -> LazyLoadedResultNonce err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray


{-| -}
alt : LazyLoadedResultNonce err a -> LazyLoadedResultNonce err a -> LazyLoadedResultNonce err a
alt fallback result =
    altLazy (always fallback) result


{-| -}
altLazy : (() -> LazyLoadedResultNonce err a) -> LazyLoadedResultNonce err a -> LazyLoadedResultNonce err a
altLazy getFallback result =
    if not (isDone result) then
        getFallback ()

    else
        result
