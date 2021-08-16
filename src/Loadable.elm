module Loadable exposing
    ( Loadable(..)
    , fromMaybe, fromResult
    , withDefault, getOrElse, getError
    , toMaybe, toResult, toTask
    , isLoading, isError, isLoaded
    , alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, flatten, join, map, map2, map3, map4, map5, mapError, mapLoading, sequenceArray, sequenceList, traverseArray, traverseList, sequenceDict, traverseDict
    )

{-|


# Model

@docs Loadable


# Constructors

@docs fromMaybe, fromResult


# Destructors

@docs withDefault, getOrElse, getError


# Folds

@docs toMaybe, toResult, toTask


# Guards

@docs isLoading, isError, isLoaded


# Combinators

@docs alt, altLazy, andMap, andThen, andThen2, andThen3, andThen4, andThen5, apply, bimap, flatten, join, map, map2, map3, map4, map5, mapError, mapLoading, sequenceArray, sequenceList, traverseArray, traverseList, sequenceDict, traverseDict

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)
import Task exposing (Task)


{-| Models data which...

1.  Needs to be loaded
2.  Begins loading immediately
3.  May fail to load

-}
type Loadable loading error value
    = Loading loading
    | Error error
    | Loaded value



-- Constructors


{-|

    fromMaybe (always "Nah dude") Nothing
    --> Error "Nah dude"

    fromMaybe (always "Nah dude") (Just "hi")
    --> Loaded "hi"

-}
fromMaybe : (() -> err) -> Maybe a -> Loadable loading err a
fromMaybe onNothing ma =
    case ma of
        Nothing ->
            Error <| onNothing ()

        Just val ->
            Loaded val


{-|

    fromResult (Result.Err "oh noes!")
    --> Error "oh noes!"

    fromResult (Result.Ok "Go")
    --> Loaded "Go"

-}
fromResult : Result err a -> Loadable loading err a
fromResult result =
    case result of
        Err err ->
            Error err

        Ok val ->
            Loaded val



-- Destructors


{-|

    Loading ()
        |> withDefault "Potato"
    --> "Potato"

    Loaded "Banana"
        |> withDefault "Potato"
    --> "Banana"

-}
withDefault : a -> Loadable loading err a -> a
withDefault fallback fa =
    case fa of
        Loaded val ->
            val

        _ ->
            fallback


{-| Attempt to access the loaded value, constructing a fallback value if the
data is not yet `Loaded`.

    Loading ()
        |> getOrElse (always "Not Loaded!")
    --> "Not Loaded!"

    Error "Something went wrong :("
        |> getOrElse (always "Not Loaded!")
    --> "Not Loaded!"

    Loaded "All done"
        |> getOrElse (always "Not Loaded!")
    --> "All done"

-}
getOrElse : (() -> a) -> Loadable loading err a -> a
getOrElse mkFallback result =
    case result of
        Loading _ ->
            mkFallback ()

        Error _ ->
            mkFallback ()

        Loaded val ->
            val


{-| Attempt to access the error in the `Error` case.

    getError (Error "Wah wah wahhh")
    --> Just "Wah wah wahhh"

    getError (Loading ())
    --> Nothing

-}
getError : Loadable loading err a -> Maybe err
getError result =
    case result of
        Error err ->
            Just err

        _ ->
            Nothing



-- Predicates


{-|

    isLoading (Loading ())
    --> True

    isLoading (Loaded "hi")
    --> False

-}
isLoading : Loadable loading err a -> Bool
isLoading data =
    case data of
        Loading _ ->
            True

        _ ->
            False


{-|

    isError (Loading ())
    --> False

    isError (Error "Uh oh")
    --> True

-}
isError : Loadable loading err a -> Bool
isError data =
    case data of
        Error _ ->
            True

        _ ->
            False


{-|

    isLoaded (Loading ())
    --> False

    isLoaded (Loaded "All done")
    --> True

-}
isLoaded : Loadable loading err a -> Bool
isLoaded data =
    case data of
        Loaded _ ->
            True

        _ ->
            False



-- Natural Transformations


{-| Transforms [Loadable](#Loadable) to a [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result),
taking a fallback **`Result`** in case of [Loading](#Loadable).

    Loading () |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Err "It was loading"

    Loading () |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "It was loading"

    Error "Oh no" |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Err "Oh no"

    Loaded "All done" |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "All done"

-}
toResult : (() -> Result err value) -> Loadable loading err value -> Result err value
toResult onLoading result =
    case result of
        Loading _ ->
            onLoading ()

        Error err ->
            Result.Err err

        Loaded val ->
            Result.Ok val


{-| Transforms [Loadable](#Loadable) to [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe),
throwing away [Loaded](Loadable) and/or [Error](#Loadable) error info.

    toMaybe (Loading ())
    --> Nothing

    toMaybe (Error "Oh no")
    --> Nothing

    toMaybe (Loaded "All done")
    --> Just "All done"

-}
toMaybe : Loadable loading err value -> Maybe value
toMaybe result =
    case result of
        Loaded val ->
            Just val

        _ ->
            Nothing


{-| Transforms [Loadable](#Loadable) to a [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task),
throwing away [Loading](Loadable) and taking a fallback [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result)
in case of [Loading](#Loadable).

    import Task

    Loading () |> toTask (\_ -> Task.succeed "It was loading")
    --> Task.succeed "It was loading"

    Loading () |> toTask (\_ -> Task.fail"It was loading")
    --> Task.fail "It was loading"

    Error "Oh no" |> toTask (\_ -> Task.succeed "It was loading")
    --> Task.fail "Oh no"

    Loaded "All done" |> toTask (\_ -> Task.succeed "It was loading")
    --> Task.succeed "All done"

-}
toTask :
    (() -> Task err value)
    -> Loadable loading err value
    -> Task err value
toTask onLoading result =
    case result of
        Loading _ ->
            onLoading ()

        Error err ->
            Task.fail err

        Loaded val ->
            Task.succeed val



-- Combinators


{-| Provide a **`Loadable`** to fall back to when the primary **`Loadable`** is
not **`Loaded`**.

    Error "Sad..."
        |> alt (Loaded "All good")
    --> Loaded "All good"

    Error "Sad..."
        |> alt (Loading "incoming...")
    --> Loading "incoming..."

    Loading "incoming..."
        |> alt (Error "Sad...")
    --> Error "Sad..."

    Loading "incoming..."
        |> alt (Loading "fallback")
    --> Loading "fallback"

    Error "sad"
        |> alt (Loaded "All done")
    --> Loaded "All done"

-}
alt : Loadable loading err a -> Loadable loading err a -> Loadable loading err a
alt fallback primary =
    altLazy (always fallback) primary


{-| Same as [alt](#alt) but the fallback value is calculated lazily
-}
altLazy : (() -> Loadable loading err a) -> Loadable loading err a -> Loadable loading err a
altLazy getFallback primary =
    if isLoaded primary then
        primary

    else
        getFallback ()


{-| Apply a function to the **`Loaded`** value, otherwise a no-op.

    Loading ()
        |> map ((+) 1)
    --> Loading ()

    Loaded 1
        |> map ((+) 1)
    --> Loaded 2

-}
map : (a -> b) -> Loadable loading err a -> Loadable loading err b
map f data =
    case data of
        Loading loading ->
            Loading loading

        Error e ->
            Error e

        Loaded a ->
            Loaded (f a)


{-| Apply a function to the **`Error`** value, otherwise a no-op.

    Loaded 1
        |> mapError String.toUpper
    --> Loaded 1

    Error "Whoops!"
        |> mapError String.toUpper
    --> Error "WHOOPS!"

-}
mapError :
    (errA -> errB)
    -> Loadable loading errA a
    -> Loadable loading errB a
mapError f result =
    case result of
        Loading loading ->
            Loading loading

        Error err ->
            Error (f err)

        Loaded val ->
            Loaded val


{-| Apply one function to the **`Error`** value, or another function to the
**`Loaded`** value, otherwise a no-op.

    Loading ()
        |> bimap String.toUpper ((+) 1)
    --> Loading ()

    Error "Whoops!"
        |> bimap String.toUpper ((+) 1)
    --> Error "WHOOPS!"

    Loaded 9000
        |> bimap String.toUpper ((+) 1)
    --> Loaded 9001

-}
bimap :
    (errA -> errB)
    -> (a -> b)
    -> Loadable loading errA a
    -> Loadable loading errB b
bimap onErr onLoaded result =
    result |> map onLoaded |> mapError onErr


{-| Apply a function to the `Loading` value, otherwise a no-op.

    Loaded "Baked Potato"
        |> mapLoading (\loading -> {loading | progress = 0})
    --> Loaded "Baked Potato"

    Loading {progress = 100}
        |> mapLoading (\loading -> {loading | progress = 0})
    --> Loading {progress = 0}

-}
mapLoading :
    (loadingA -> loadingB)
    -> Loadable loadingA err a
    -> Loadable loadingB err a
mapLoading f result =
    case result of
        Loading loading ->
            Loading (f loading)

        Error err ->
            Error err

        Loaded val ->
            Loaded val


{-| Unnests a **`Loadable`** from within another `Loadable`.

    flatten (Loaded (Loaded "It's loaded!"))
    --> Loaded "It's loaded!"

    flatten (Loading ())
    --> Loading ()

-}
flatten : Loadable loading err (Loadable loading err a) -> Loadable loading err a
flatten data =
    case data of
        Loading loading ->
            Loading loading

        Error err ->
            Error err

        Loaded nested ->
            nested


{-| Alias for **[flatten](#flatten)**
-}
join : Loadable loading err (Loadable loading err a) -> Loadable loading err a
join =
    flatten


{-| Applies a function in a **`Loadable`** to a value in another **`Loadable`**.

    type alias BlogPost =
        { content : String
        , comments : List String
        }

    Loaded BlogPost
        |> apply (Loaded "Hello, world!")
        |> apply (Loaded ["Nice!"])
    --> Loaded { content = "Hello, world!", comments = ["Nice!"]}

    Loaded BlogPost
        |> apply (Loaded "Hello, world!")
        |> apply (Loading "comments incoming...")
    --> Loading "comments incoming..."

-}
apply :
    Loadable loading err a
    -> Loadable loading err (a -> b)
    -> Loadable loading err b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| An alias for **[apply](#apply)**
-}
andMap : Loadable loading err a -> Loadable loading err (a -> b) -> Loadable loading err b
andMap =
    apply


{-| Apply a binary function to its arguments wrapped in `Loadable`s.
Essentially a shorthand for **[apply](#apply)**.

    type alias BlogPost =
        { content : String
        , comments : List String
        }

    map2 BlogPost
        (Loaded "Hello, world!")
        (Loaded ["Nice!"])
    --> Loaded
    -->     { content = "Hello, world!"
    -->     , comments = ["Nice!"]
    -->     }

    map2 BlogPost
        (Loaded "Hello, world!")
        (Loading "Comments incoming...")
    --> Loading "Comments incoming..."

    map2 BlogPost
        (Loaded "Hello, world!")
        (Error "Bad comments")
    --> Error "Bad comments"

-}
map2 :
    (a -> b -> c)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
map2 f fa ga =
    Loaded f
        |> apply fa
        |> apply ga


{-| Apply a trinary function to its arguments wrapped in `Loadable`s.
Essentially a shorthand for **[apply](#apply)**.

    type alias BlogPost =
        { content : String
        , comments : List String
        , likes : Int
        }

    map3 BlogPost
        (Loaded "Hello, world!")
        (Loaded ["Nice!"])
        (Loaded 55)
    --> Loaded
    -->     { content = "Hello, world!"
    -->     , comments = ["Nice!"]
    -->     , likes = 55
    -->     }

    map3 BlogPost
        (Loaded "Hello, world!")
        (Loaded ["Nice!"])
        (Loading "Loading likes...")
    --> Loading "Loading likes..."

    map3 BlogPost
        (Loaded "Hello, world!")
        (Loading "Comments incoming...")
        (Error "Negative likes?")
    --> Loading "Comments incoming..."

-}
map3 :
    (a -> b -> c -> d)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
map3 f fa ga ha =
    Loaded f
        |> apply fa
        |> apply ga
        |> apply ha


{-| Like **[map2](#map2)** and **[map3](#map3)**. See their docs for more info.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
    -> Loadable loading err e
map4 f fa ga ha ia =
    Loaded f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia


{-| Like **[map2](#map2)** and **[map3](#map3)**. See their docs for more info.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
    -> Loadable loading err e
    -> Loadable loading err f
map5 f fa ga ha ia ja =
    Loaded f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| Construct a `Loadable` based on the result of a prior `Loadable`.

    data : Loadable () String Int
    data = Loaded 100

    data |> andThen
        (\n -> if n >= 100 then Loaded "Good job" else Error "Lame")
    --> Loaded "Good job"

    data |> andThen
        (\n -> if n > 100 then Loaded "Good job" else Error "Lame")
    --> Error "Lame"

    data |> andThen
        (\n -> if n > 100 then Loaded "Good job" else Loading ())
    --> Loading ()

-}
andThen :
    (a -> Loadable loading err b)
    -> Loadable loading err a
    -> Loadable loading err b
andThen f fa =
    flatten <| map f fa


{-| Like map2 but for **[andThen](#andThen)**
-}
andThen2 :
    (a -> b -> Loadable loading err c)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
andThen2 f fa fb =
    Loaded f
        |> apply fa
        |> apply fb
        |> flatten


{-| Like map3 but for **[andThen](#andThen)**
-}
andThen3 :
    (a -> b -> c -> Loadable loading err d)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
andThen3 f fa fb fc =
    Loaded f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| Like map4 but for **[andThen](#andThen)**
-}
andThen4 :
    (a -> b -> c -> d -> Loadable loading err e)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
    -> Loadable loading err e
andThen4 f fa fb fc fd =
    Loaded f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> flatten


{-| Like map5 but for **[andThen](#andThen)**
-}
andThen5 :
    (a -> b -> c -> d -> e -> Loadable loading err f)
    -> Loadable loading err a
    -> Loadable loading err b
    -> Loadable loading err c
    -> Loadable loading err d
    -> Loadable loading err e
    -> Loadable loading err f
andThen5 f fa fb fc fd fe =
    Loaded f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| Turn a **`List (Loadable l e a)`** into a **`Loadable l e (List a)`**,
resulting in the first non-**`Loaded`** element. Only if all elements are
**`Loaded`** will the result be **`Loaded`**.

    sequenceList [ Loaded 1 , Loaded 2, Loaded 3 ]
    --> Loaded [1, 2, 3]

    sequenceList [ Loaded 1 , Loading (), Loaded 3 ]
    --> Loading ()

    sequenceList [ Loaded 1, Loading (), Error "D'oh!" ]
    --> Loading ()

    sequenceList [ Loaded 1,  Error "D'oh!", Loading () ]
    --> Error "D'oh!"

-}
sequenceList : List (Loadable loading err a) -> Loadable loading err (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Loaded []) fas


{-| Turn a **`List (Loadable l e a)`** into a **`Loadable l e (List b)`** by
performing an **[andThen](#andThen)** on each element before running
[sequenceList](#sequenceList).

    [ Loaded 1 , Loaded 2, Loaded 3 ]
        |> traverseList (Loaded << String.fromInt)
    --> Loaded ["1", "2", "3"]

    [ Loaded 1 , Loading (), Loaded 3 ]
        |> traverseList (Loaded << String.fromInt)
    --> Loading ()

    [ Loaded 1, Loading (), Error "D'oh!" ]
        |> traverseList (Loaded << String.fromInt)
    --> Loading ()

    [ Loaded 1,  Error "D'oh!", Loading () ]
        |> traverseList (Loaded << String.fromInt)
    --> Error "D'oh!"

-}
traverseList : (a -> Loadable loading err b) -> List (Loadable loading err a) -> Loadable loading err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| Turn a **`Array (Loadable l e a)`** into a **`Loadable l e (Array a)`**,
resulting in the first non-**`Loaded`** element. Only if all elements are
**`Loaded`** will the result be **`Loaded`**.

    import Array

    sequenceArray (Array.fromList [ Loaded 1 , Loaded 2, Loaded 3 ])
    --> Loaded (Array.fromList [1, 2, 3])

    sequenceArray (Array.fromList [ Loaded 1 , Loading (), Loaded 3 ])
    --> Loading ()

    sequenceArray (Array.fromList [ Loaded 1, Loading (), Error "D'oh!" ])
    --> Loading ()

    sequenceArray (Array.fromList [ Loaded 1,  Error "D'oh!", Loading () ])
    --> Error "D'oh!"

-}
sequenceArray : Array (Loadable loading err a) -> Loadable loading err (Array a)
sequenceArray fas =
    sequenceList (Array.toList fas) |> map Array.fromList


{-| Turn an **`Array (Loadable l e a)`** into a **`Loadable l e (Array b)`** by
performing an **[andThen](#andThen)** on each element before running
[sequenceList](#sequenceList).

    import Array

    Array.fromList [ Loaded 1 , Loaded 2, Loaded 3 ]
        |> traverseArray (Loaded << String.fromInt)
    --> Loaded (Array.fromList ["1", "2", "3"])

    Array.fromList [ Loaded 1 , Loading (), Loaded 3 ]
        |> traverseArray (Loaded << String.fromInt)
    --> Loading ()

    Array.fromList [ Loaded 1, Loading (), Error "D'oh!" ]
        |> traverseArray (Loaded << String.fromInt)
    --> Loading ()

    Array.fromList [ Loaded 1,  Error "D'oh!", Loading () ]
        |> traverseArray (Loaded << String.fromInt)
    --> Error "D'oh!"

-}
traverseArray : (a -> Loadable loading err b) -> Array (Loadable loading err a) -> Loadable loading err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray


{-| Turn a **`List (Loadable l e a)`** into a **`Loadable l e (List a)`**,
resulting in the first non-**`Loaded`** element. Only if all elements are
**`Loaded`** will the result be **`Loaded`**.

    import Dict

    sequenceDict <|
         Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1 , Loaded 2, Loaded 3 ]
    --> Loaded <| Dict.fromList (List.indexedMap Tuple.pair [ 1 , 2, 3 ])

    sequenceDict <|
         Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1 , Loading (), Loaded 3 ]
    --> Loading ()

    sequenceDict <|
         Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1, Loading (), Error "D'oh!" ]
    --> Loading ()

    sequenceDict <|
         Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1,  Error "D'oh!", Loading () ]
    --> Error "D'oh!"

-}
sequenceDict :
    Dict comparable (Loadable loading err a)
    -> Loadable loading err (Dict comparable a)
sequenceDict fas =
    Dict.foldr (map2 << Dict.insert) (Loaded Dict.empty) fas


{-| Turn a **`Dict k (Loadable l e a)`** into a **`Loadable l e (Dict k b)`** by
performing an **[andThen](#andThen)** on each element before running
[sequenceDict](#sequenceDict).

    import Dict

    traverseDict (\_ v -> Loaded (String.fromInt v)) <|
        Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1 , Loaded 2, Loaded 3 ]
    --> Loaded <| Dict.fromList <| List.indexedMap Tuple.pair [ "1" , "2", "3" ]

    traverseDict (\_ v -> Loaded (String.fromInt v)) <|
        Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1 , Loading (), Loaded 3 ]
    --> Loading ()

    traverseDict (\_ v -> Loaded (String.fromInt v)) <|
        Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1, Loading (), Error "D'oh!" ]
    --> Loading ()

    traverseDict (\_ v -> Loaded (String.fromInt v)) <|
        Dict.fromList <|
            List.indexedMap Tuple.pair [ Loaded 1,  Error "D'oh!", Loading () ]
    --> Error "D'oh!"

-}
traverseDict :
    (comparable -> a -> Loadable loading err b)
    -> Dict comparable (Loadable loading err a)
    -> Loadable loading err (Dict comparable b)
traverseDict f =
    Dict.map (andThen << f) >> sequenceDict
