# elm-loadable

Custom types for representing the possible states of loadable data

## Installation

```sh
elm install derrickbeining/elm-loadable
```

## What?

**`elm-loadable`** provides the **`Loadable`** and **`LazyLoadable`** custom
types to help you model the states your app can be in when you have data to
load from somewhere. It also provides **`Reloadable`** and **`LazyReloadable`**,
which are type-aliased versions of **`Loadable`** and **`LazyLoadable`**
configured to represent the additional states involved in reloading.

The reality is there are numerous kinds data-loading situations each with
their own special requirements. It's futile to try to make a one-size-fits-all
data type to model all of them. But it also feels not-quite-right to be
hand-rolling one-off custom types for every scenario. So **`elm-loadable`**
attempts to strike a balance by providing only a few types which handle the
most basic/common scenarios out-of-the-box, but which are also sufficiently
generic to allow for virtually limitless additional custom states to be added
on through type application. See the [#examples](#examples) section below to see what that
might look like.


## Why?
This package is an attempt to make up for what is lacking in
[krisajenkins/remotedata](https://package.elm-lang.org/packages/krisajenkins/remotedata/latest/RemoteData)
and similar packages. While nicer than **`Maybe (Result err val)`**,
**`RemoteData`** nevertheless is not the right representation for every
data-loading scenario and shouldn't be shoe-horned as such. If you've used
**`RemoteData`** for any non-trivial applications, you are probably already
aware of where it isn't particularly helpful.

For example, here a few things **`RemoteData`** can't represent:

1. Refreshing/reloading/retrying your data without throwing away the previously loaded value, the previously encountered error, or both.
2. Tracking file upload/download progress.
3. Data that begins loading _immediately_ so that it's never in the **`NotAsked`** state.

In order to enable you to model these and many other data-loading scenarios in which you might find yourself, this package exposes two custom types: **`Loadable`** and **`LazyLoadable`**. The only difference between them is that **`Loadable`** models eager-loaded data (starts in the **`Loading`** state) and **`LazyLoadable`** models data that remains in **`Initial`** state until forced to begin **`Loading`**.

## Differences from **`RemoteData`**

At first glance, **`RemoteData`** doesn't appear all that different from these types. Let's compare it with **`LazyLoadable`**:

```elm
type RemoteData e a
    = NotAsked
    | Loading
    | Failure e
    | Success a

type LazyLoadable loading error value
    = Initial
    | Loading loading
    | Error error
    | Loaded value

```

Aside from inconsequential naming choices, the only difference is that **`LazyLoadable`** has an additional type parameter called **`loading`** which the **`Loading`** variant holds. However, this one little change makes many other states representable.


To demonstrate the possibilities, examples are provided below to illustrate how you might tailor (Lazy)Loadable to your various needs:

## Examples

### **`RemoteData`** equivalent

Adding no extra info to the `Loading` variant makes `LazyLodable` equivalent to
`RemoteData`.

```elm
type alias RemoteData e a =
    LazyLoadable () e a
```

### No **`Error`** state needed

Disable the `Error` variant with `Never` if you don't want to hold errors
in your model.

```elm
type alias Model =
    { data : LazyLoadable LoadingMeta Never Entity }
```

### Track file upload/download progress

Keep track of loading progress on the `Loading` variant:

```elm
type alias Model =
    { file : LazyLoadable (ProgressPercent Int) Http.Error () }
```

### Track retry count

Use the `loading` type param to keep track of how many consecutive retries
have occurred so you can perform exponential back off and quit at some defined
limit.

```elm
type alias Model =
  { data : Loadable (Retries Int) Http.Error Token }
```

### Lossless Reloading

Use the `loading` and `error` type params to keep track of the last known
`Loaded` value and/or `Error` so you can reload or retry while still displaying
the last known state to the user.

```elm
type alias LazyReloadable loading error value =
    LazyLoadable
        -- prev Error, prev Loaded, whatever you want
        ( Maybe error, Maybe value, loading )
        -- current error, prev Loaded
        ( error, Maybe value )
        value
```
