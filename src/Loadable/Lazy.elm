module Loadable.Lazy exposing
    ( LazyLoadable(..)
    , fromMaybe
    , fromResult
    , fromLazyReloadable
    , fromLoadable
    , fromReloadable
    , withDefault, getOrElse, getError
    , toLazyReloadable
    , toLoadable
    , toReloadable
    , toList
    , toArray
    , toSet
    , toMaybe
    , toResult
    , toTask
    , isInitial
    , isLoading
    , isError
    , isLoaded
    , alt
    , altLazy
    , andMap
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , andThen5
    , apply
    , bimap
    , mapLoading
    , flatten
    , join
    , map
    , map2
    , map3
    , map4
    , map5
    , mapError
    , sequenceArray
    , sequenceList
    , traverseArray
    , traverseList
    )

{-|


# Model

@docs LazyLoadable


# Constructors

@docs fromMaybe
@docs fromResult
@docs fromLazyReloadable
@docs fromLoadable
@docs fromReloadable


# Destructors

@docs withDefault, getOrElse, getError


# Natural Transformations

@docs toLazyReloadable
@docs toLoadable
@docs toReloadable
@docs toList
@docs toArray
@docs toSet
@docs toMaybe
@docs toResult
@docs toTask


# Guards

@docs isInitial
@docs isLoading
@docs isError
@docs isLoaded


# Combinators

@docs alt
@docs altLazy
@docs andMap
@docs andThen
@docs andThen2
@docs andThen3
@docs andThen4
@docs andThen5
@docs apply
@docs bimap
@docs mapLoading
@docs flatten
@docs join
@docs map
@docs map2
@docs map3
@docs map4
@docs map5
@docs mapError
@docs sequenceArray
@docs sequenceList
@docs traverseArray
@docs traverseList

-}

import Array exposing (Array)
import Loadable exposing (Loadable(..))
import Reloadable exposing (Reloadable)
import Set exposing (Set)
import Task exposing (Task)


{-| Models data which...

1.  Needs to be loaded
2.  Does **not** begin loading immediately and may never begin to load
3.  May fail to load

-}
type LazyLoadable loading error value
    = Initial
    | Loading loading
    | Error error
    | Loaded value


{-| Attempt to convert a `LazyLoadable` value to a `Loadable` if possible
-}
toLoadable : LazyLoadable loading err val -> Maybe (Loadable loading err val)
toLoadable lazyLoadable =
    case lazyLoadable of
        Initial ->
            Nothing

        Loading loading_ ->
            Just <| Loadable.Loading loading_

        Error err ->
            Just <| Loadable.Error err

        Loaded val ->
            Just <| Loadable.Loaded val


{-| Convert a `Loadable` value to a `LazyLoadable`
-}
fromLoadable : Loadable loading err val -> LazyLoadable loading err val
fromLoadable loadable =
    case loadable of
        Loadable.Loading loading_ ->
            Loading loading_

        Loadable.Error err ->
            Error err

        Loadable.Loaded val ->
            Loaded val


{-| Convert a `LazyLoadable` value to a `LazyReloadable`
-}
toLazyReloadable :
    LazyLoadable loading err val
    ->
        LazyLoadable
            ( Maybe err, Maybe value, loading )
            ( err, Maybe val )
            val
toLazyReloadable lazyLoadable =
    case lazyLoadable of
        Initial ->
            Initial

        Loading loading_ ->
            Loading ( Nothing, Nothing, loading_ )

        Error err ->
            Error ( err, Nothing )

        Loaded val ->
            Loaded val


{-| Convert a `LazyReloadable` value to a `LazyLoadable`
-}
fromLazyReloadable :
    LazyLoadable
        ( Maybe err, Maybe val, loading )
        ( err, Maybe val )
        val
    -> LazyLoadable loading err val
fromLazyReloadable lazyReloadable =
    case lazyReloadable of
        Initial ->
            Initial

        Loading ( _, _, loading_ ) ->
            Loading loading_

        Error ( err, _ ) ->
            Error err

        Loaded val ->
            Loaded val


{-| Convert a `LazyLoadable` value to a `Reloadable` if possible
-}
toReloadable : LazyLoadable loading err val -> Maybe (Reloadable loading err val)
toReloadable lazyLoadable =
    case lazyLoadable of
        Initial ->
            Nothing

        Loading loading_ ->
            Just <| Reloadable.loading Nothing Nothing loading_

        Error err ->
            Just <| Reloadable.error err Nothing

        Loaded val ->
            Just <| Reloadable.loaded val


{-| Convert a `Reloadable` value to a `LazyLoadable`
-}
fromReloadable : Reloadable loading err val -> LazyLoadable loading err val
fromReloadable reloadable =
    case reloadable of
        Loadable.Loading ( _, _, loading_ ) ->
            Loading loading_

        Loadable.Error ( err, _ ) ->
            Error err

        Loadable.Loaded val ->
            Loaded val



-- Constructors


{-|

    fromMaybe (always "Nah dude") Nothing
    --> Error "Nah dude"

    fromMaybe (always "Nah dude") (Just "hi")
    --> Loaded "hi"

-}
fromMaybe : (() -> err) -> Maybe a -> LazyLoadable loading err a
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
fromResult : Result err a -> LazyLoadable loading err a
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
withDefault : a -> LazyLoadable loading err a -> a
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
getOrElse : (() -> a) -> LazyLoadable loading err a -> a
getOrElse mkFallback result =
    case result of
        Initial ->
            mkFallback ()

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
getError : LazyLoadable loading err a -> Maybe err
getError result =
    case result of
        Error err ->
            Just err

        _ ->
            Nothing



-- Predicates


{-|

    isInitial (Initial)
    --> True

    isInitial (Loaded "hi")
    --> False

-}
isInitial : LazyLoadable loading err a -> Bool
isInitial data =
    case data of
        Initial ->
            True

        _ ->
            False


{-|

    isLoading (Loading ())
    --> True

    isLoading (Loaded "hi")
    --> False

-}
isLoading : LazyLoadable loading err a -> Bool
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
isError : LazyLoadable loading err a -> Bool
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
isLoaded : LazyLoadable loading err a -> Bool
isLoaded data =
    case data of
        Loaded _ ->
            True

        _ ->
            False



-- Natural Transformations


{-| Transforms [LazyLoadable](#LazyLoadable) to a `List` (empty if not `Loaded`)

    toList (Loading ())
    --> []

    toList (Loaded "sup")
    ---> ["sup"])

-}
toList : LazyLoadable loading error value -> List value
toList data =
    case data of
        Loaded val ->
            [ val ]

        _ ->
            []


{-| Transforms [LazyLoadable](#LazyLoadable) to a `Array` (empty if not `Loaded`)

    import Array

    toArray (Loading ())
    --> Array.fromList []

    toArray (Loaded "sup")
    ---> Array.fromList ["sup"]

-}
toArray : LazyLoadable loading error value -> Array value
toArray data =
    case data of
        Loaded val ->
            Array.fromList [ val ]

        _ ->
            Array.empty


{-| Transforms [LazyLoadable](#LazyLoadable) to a `Set` (empty if not `Loaded`)

    import Set

    toSet (Loading ())
    --> Set.fromList []

    toSet (Loaded "sup")
    ---> Set.fromList ["sup"])

-}
toSet : LazyLoadable loading error comparable -> Set comparable
toSet data =
    case data of
        Loaded val ->
            Set.fromList [ val ]

        _ ->
            Set.empty


{-| Transforms [LazyLoadable](#LazyLoadable) to a [Result](https://package.elm-lang.org/packages/elm/core/latest/Result#Result),
taking a fallback **`Result`** in case of [Loading](#LazyLoadable).

    Initial |> toResult (\_ -> Result.Err "It hasn't started yet")
    --> Result.Err "It hasn't started yet"

    Initial |> toResult (\_ -> Result.Ok "It hasn't started yet")
    --> Result.Ok "It hasn't started yet"

    Loading () |> toResult (\_ -> Result.Err "It was loading")
    --> Result.Err "It was loading"

    Loading () |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "It was loading"

    Error "Oh no" |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Err "Oh no"

    Loaded "All done" |> toResult (\_ -> Result.Ok "It was loading")
    --> Result.Ok "All done"

-}
toResult : (() -> Result err value) -> LazyLoadable loading err value -> Result err value
toResult mkFallback result =
    case result of
        Error err ->
            Result.Err err

        Loaded val ->
            Result.Ok val

        _ ->
            mkFallback ()


{-| Transforms [LazyLoadable](#LazyLoadable) to [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe#Maybe),
throwing away [Loaded](LazyLoadable) and/or [Error](#LazyLoadable) error info.

    toMaybe (Loading ())
    --> Nothing

    toMaybe (Error "Oh no")
    --> Nothing

    toMaybe (Loaded "All done")
    --> Just "All done"

-}
toMaybe : LazyLoadable loading err a -> Maybe a
toMaybe result =
    case result of
        Loaded val ->
            Just val

        _ ->
            Nothing


{-| Transforms [LazyLoadable](#LazyLoadable) to a [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task),
taking a fallback [Task](https://package.elm-lang.org/packages/elm/core/latest/Task#Task)
in case of [Initial](#LazyLoadable) or [Loading](#LazyLoadable).

    import Task

    Initial |> toTask (\_ -> Task.succeed "It hasn't started yet")
    --> Task.succeed "It hasn't started yet"

    Initial |> toTask (\_ -> Task.fail"It hasn't started yet")
    --> Task.fail "It hasn't started yet"

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
    -> LazyLoadable loading err value
    -> Task err value
toTask mkFallback result =
    case result of
        Error err ->
            Task.fail err

        Loaded val ->
            Task.succeed val

        _ ->
            mkFallback ()



-- Combinators


{-| Provide a **`LazyLoadable`** to fall back to when the primary **`LazyLoadable`** is
not **`Loaded`**.

    Initial
        |> alt (Loaded "All good")
    --> Loaded "All good"

    Initial
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

    Loaded "First"
        |> alt (Loaded "Second")
    --> Loaded "First"

-}
alt : LazyLoadable loading err a -> LazyLoadable loading err a -> LazyLoadable loading err a
alt fallback primary =
    altLazy (always fallback) primary


{-| Same as [alt](#alt) but the fallback value is calculated lazily
-}
altLazy : (() -> LazyLoadable loading err a) -> LazyLoadable loading err a -> LazyLoadable loading err a
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
map : (a -> b) -> LazyLoadable loading err a -> LazyLoadable loading err b
map f data =
    case data of
        Initial ->
            Initial

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
    -> LazyLoadable loading errA a
    -> LazyLoadable loading errB a
mapError f result =
    case result of
        Initial ->
            Initial

        Loading loading ->
            Loading loading

        Error err ->
            Error (f err)

        Loaded val ->
            Loaded val


{-| Apply the first function to the **`Error`** value, or the second function
to the **`Loaded`** value, otherwise a no-op.

    Initial
        |> bimap String.toUpper ((+) 1)
    --> Initial

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
    -> LazyLoadable loading errA a
    -> LazyLoadable loading errB b
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
    -> LazyLoadable loadingA err a
    -> LazyLoadable loadingB err a
mapLoading f result =
    case result of
        Initial ->
            Initial

        Loading loading ->
            Loading (f loading)

        Error err ->
            Error err

        Loaded val ->
            Loaded val


{-| Unnests a **`LazyLoadable`** from within another `LazyLoadable`.

    flatten (Loaded (Loaded "It's loaded!"))
    --> Loaded "It's loaded!"

    flatten (Loading ())
    --> Loading ()

-}
flatten : LazyLoadable loading err (LazyLoadable loading err a) -> LazyLoadable loading err a
flatten data =
    case data of
        Initial ->
            Initial

        Loading loading ->
            Loading loading

        Error err ->
            Error err

        Loaded nested ->
            nested


{-| Alias for **[flatten](#flatten)**
-}
join : LazyLoadable loading err (LazyLoadable loading err a) -> LazyLoadable loading err a
join =
    flatten


{-| Applies a function in a **`LazyLoadable`** to a value in another **`LazyLoadable`**.

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
    LazyLoadable loading err a
    -> LazyLoadable loading err (a -> b)
    -> LazyLoadable loading err b
apply fa fAtoB =
    flatten <| map (\f -> fa |> map (\a -> f a)) fAtoB


{-| An alias for **[apply](#apply)**
-}
andMap : LazyLoadable loading err a -> LazyLoadable loading err (a -> b) -> LazyLoadable loading err b
andMap =
    apply


{-| Apply a binary function to its arguments wrapped in `LazyLoadable`s.
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
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
map2 f fa ga =
    Loaded f
        |> apply fa
        |> apply ga


{-| Apply a trinary function to its arguments wrapped in `LazyLoadable`s.
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
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
map3 f fa ga ha =
    Loaded f
        |> apply fa
        |> apply ga
        |> apply ha


{-| Like **[map2](#map2)** and **[map3](#map3)**. See their docs for more info.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
    -> LazyLoadable loading err e
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
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
    -> LazyLoadable loading err e
    -> LazyLoadable loading err f
map5 f fa ga ha ia ja =
    Loaded f
        |> apply fa
        |> apply ga
        |> apply ha
        |> apply ia
        |> apply ja


{-| Construct a `LazyLoadable` based on the result of a prior `LazyLoadable`.

    data : LazyLoadable () String Int
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
    (a -> LazyLoadable loading err b)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
andThen f fa =
    flatten <| map f fa


{-| Like map2 but for **[andThen](#andThen)**
-}
andThen2 :
    (a -> b -> LazyLoadable loading err c)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
andThen2 f fa fb =
    Loaded f
        |> apply fa
        |> apply fb
        |> flatten


{-| Like map3 but for **[andThen](#andThen)**
-}
andThen3 :
    (a -> b -> c -> LazyLoadable loading err d)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
andThen3 f fa fb fc =
    Loaded f
        |> apply fa
        |> apply fb
        |> apply fc
        |> flatten


{-| Like map4 but for **[andThen](#andThen)**
-}
andThen4 :
    (a -> b -> c -> d -> LazyLoadable loading err e)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
    -> LazyLoadable loading err e
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
    (a -> b -> c -> d -> e -> LazyLoadable loading err f)
    -> LazyLoadable loading err a
    -> LazyLoadable loading err b
    -> LazyLoadable loading err c
    -> LazyLoadable loading err d
    -> LazyLoadable loading err e
    -> LazyLoadable loading err f
andThen5 f fa fb fc fd fe =
    Loaded f
        |> apply fa
        |> apply fb
        |> apply fc
        |> apply fd
        |> apply fe
        |> flatten


{-| Turn a **`List (LazyLoadable l e a)`** into a **`LazyLoadable l e (List a)`**,
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
sequenceList : List (LazyLoadable loading err a) -> LazyLoadable loading err (List a)
sequenceList fas =
    List.foldr (map2 (::)) (Loaded []) fas


{-| Turn a **`List (LazyLoadable l e a)`** into a **`LazyLoadable l e (List b)`** by
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
traverseList : (a -> LazyLoadable loading err b) -> List (LazyLoadable loading err a) -> LazyLoadable loading err (List b)
traverseList f =
    List.map (andThen f) >> sequenceList


{-| Turn a **`Array (LazyLoadable l e a)`** into a **`LazyLoadable l e (Array a)`**,
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
sequenceArray : Array (LazyLoadable loading err a) -> LazyLoadable loading err (Array a)
sequenceArray fas =
    sequenceList (Array.toList fas) |> map Array.fromList


{-| Turn an **`Array (LazyLoadable l e a)`** into a **`LazyLoadable l e (Array b)`** by
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
traverseArray : (a -> LazyLoadable loading err b) -> Array (LazyLoadable loading err a) -> LazyLoadable loading err (Array b)
traverseArray f =
    Array.map (andThen f) >> sequenceArray
