# Design

`Json` encoding is relatively straight-forward, so we'll just encode things to `Json` and `stringify` from there.

## Primitive

At the end of the day, `Json` decoding is a function of `Json -> Maybe a` where either the decoder failed, `Nothing`, or succeeded, `Just a`.

```purs
type PrimitiveJsonDecoder = Json -> Maybe a
```

## Debuggable

### What went wrong

This type is ideal when one is code-generating a codec based on some other specification because presumably the codec is correct already. However, if one is handwriting a codec, this is problematic because a mistake in the codec will fail on valid data. Since the failure is `Nothing`, no information is provided about where the failure happened, nor why.

If we migrate from `Maybe` to its isomorphic type `Either Unit`, we keep the same function but now allow for an error type with more information. 

```purs
type PrimitiveJsonDecoder' = Json -> Either Unit a
```

Making that `Unit` type polymorphic via `e`, we now get

```purs
type DebuggableJsonDecoder e a = Json -> Either e a
```

But what should this `e` be? Previously, it was `Unit` which didn't get us far. Now, it can be one of two types:
- an unstructured error (e.g. `String`)
- a structured error (e.g. `JsonDecodeError`)

Typically, a structured error will be built during decoding and then, on decoding failure, be printed back to a `String` that is then logged. There may be a case in which the structure is used to produce a non-`String` value, but that is likely rare. Thus, in the former case, the structure exists to inform how to pretty-print that `String`. Its usage in the latter case is harder to describe due to its rare if any usage. 

So, to anwer this question, we must answer how the error will be used:
1. If the usage of the error is to log a non-pretty-printed `String` value, then just use `String`. By "non-pretty-printed `String`", I mean a `String` value where some non trivial computation is needed to properly print it, usually to make its appearance more readable.
1. Otherwise, use a structured error

### Where it went wrong

Regardless of which error above is chosen, the error is not helpful unless one knows _where_ it occurred. Thus, one needs to include the path taken in the JSON to arrive at the location where the error occurred. There are two ways to do this, echoing the idea of a structured and unstructured error.
1. an unstructured path (e.g. `String`)
1. a structured path (e.g. `JsonPath`)

In the unstructured approach via `String`, one can prepend the path information to the error. For example, every time we traverse down a key or index, we append `"under key, " <> show key <> ", "` or `"under index, " <> show idx <> ", "` to the error.

The structured approach can be represented in a few different ways, but something of the following would suffice:
```purs
type JsonPath_1 = List (Either Int String)
  -- Nil = Root
  -- Cons (Left i) -- "under index, " <> show i <> ", "
  -- Cons (Right String) -- "under key, " <> show k <> ", "

-- A more human-readable encoding would be:
data JsonPath
  = AtKey String JsonPath
  | AtIndex Int JsonPath
  | AtRoot -- optional
```

A structured error could then represent its full error as `Tuple JsonPath e` where `e` is the structured error that occurs at the location indicated by the `JsonPath`.

## Write once, debug as needed

When Json decoding succeeds, any overhead from the possible error message is pointless. But when it does fail, then having clear errors would be nice. Ideally, we could write out decoders once and only pay for the overhead when the failure happens. Since decoding happens within a Monad, and we want to swap in the implementation depending on the situation we're in, we need to use a type class. For example, a hypothetical type class `IsJsonDecoder f` describes some monadic type `f` that can be used to produce a pretty-printed error message or ignore such things and just be a wrapper over `Maybe`. This then allows the following workflow:
1. write a decoder once
1. run the codec using an error type like `Maybe` above to prioritize speed
1. upon failure run the decoder again using an information-rich error

With the advent of Visible Type Applications (VTAs), we could write the following code...
```purs
decodeX :: forall @f a. Applicative f => (forall @g. IsJsonDecoder g => Json -> g a) -> Json -> f a
decodeX decoder j = case decoder @Maybe j of
  Just a -> pure a
  Nothing -> decoder @f j
```

... which reads as:
1. Decode the JSON using the fast codec via Maybe. On the happy path of having a `Json` value that is valid, this is the fastest way to get an `a` out of it. When it succeeds, wrap it within the `f` monad.
1. If the fast one fails, decode the JSON again but using a decoder monad `f` with more descriptive error messages.

Unfortunately, the above approach doesn't get us what we want due to type class dictionary overhead. While using `Maybe` should be fast, the type class dictionary makes it slower than just using `Either` as-is. Even if we did have specialization, this approach increases one's bundle sizes because the same decoders must be stored once for each monad. 

So, this library tries to get the best tradeoff via `Either DecodeError`:
- slightly slower than just using `Maybe`
- still faster than `Either JsonDecodeError`
- still fairly debuggable
- still allows custom error messages (unlike `JsonDecodeError`)

### Allowing Hints

It might be useful to allow the end-user to add hints at various points. For example, adding "while decoding type `Foo`" to the final error message. This can add context to the intent of the decoder at specific points in the JSON path.

While desirable, the problem is how to print the resulting error message in light of accumulated errors. One strategy is to print the hint to the right of the path.

```
ROOT."path"."to"."some"."place" (here is a hint)
  [0] - Expected Array but got Null
```

But what happens when there is an interleaving of JSON path and hint, such that one gets somethng like this:
```
ROOT."path" <hint> ."to" <hint> ."some" <hint> ."place" <hint>
```

The problem here is that the hints drown out the full JSON path.

Another strategy might be to print the hint and then indent and print the error it wraps:
```
ROOT."path"
  <hint>
    ."to"
      <hint>
        ."some"
          <hint>
            ."place"
              <hint>
                [0] Expected Array but got Null
```

While this can work, it again drowns out the full JSON path. Moreover, too many hints everywhere could do more harm than good. 

So, I chose not to include hints in the `DecodeError` type (shown next). Since one will often need to add logging statements to the decoder to see what's going on, they can abuse the `AtKey` constructor to insert such hint information temporarily exactly where they need it, debug the problem, and then remove the abuse.

### Accumulating Errors

Another design decision we could make is whether to report all errors in a json decode pass rather than just the first one. In other words, if a JSON value is missing two keys that are required, rather than only reporting that the first key is missing in the error message, the error would report both keys.

While this library initially did things in that way, the resulting codec was slower on the happy decoding path than other libraries. So, I removed this feature from the library.

## DecodeError

```purs
data DecodeError
  -- path information
  = AtKey String DecodeError
  | AtIndex Int DecodeError
  -- leaf error
  | DecodeError String
```

Via [the benchmarks](./bench/results), I learned
- using an error type of `String`, adding path information via`lmap (append $ "." <> show key)`, and printing via `identity` is slower than other methods. It seems the overhead of `show` is what causes the slow down.
- using an error type of `List String`, adding path information via `lmap (Cons $ "." <> show key)`, and printing via `fold` is slower than other methods.
- using an error type of `DecodeError`, adding path information via `lmap (AtKey key)`, and printing via `printDecodeError` is the current fastest known method if one wants errors. If errors are not desired then `Maybe a` is the fastest decoding monad.

### Inline Directives

While adding inline directives for encoding is straight forward, doing so for decoding is not. As I learned while working on the `snapshots` folder and then later confirmed in a conversation with Nathan Faubion, error handling in general is exponential if you inline all error handling paths. For example, [ToRecordInlines.purs](./snapshots/Snapshot/ToRecordInlines.purs) currently produces [its snapshot](./snapshots/Snapshot/ToRecordInlines-snapshot.js) of 78 LOC. After inlining both the type class dictionary `toRecordObjCons`, its type class member `toRecordObj`, and the `to*` functions (e.g. `toRequired`, `toOptionRename`, etc), it produced a file containing ~42,000 LOC.
