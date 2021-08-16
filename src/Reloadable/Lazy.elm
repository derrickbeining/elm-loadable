module Reloadable.Lazy exposing (LazyReloadable)

{-| This module provides only a type alias with no additional functions. See the
**[Loadable.Lazy](Loadable.Lazy)** module for available functions.


# Model

@docs LazyReloadable

-}

import Loadable.Lazy exposing (LazyLoadable(..))


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
