# purescript-json-codecs

A JSON codec library that supports value-based and configurable typeclass-based json codecs

# Design

Gripes with other JSON codec libraries (i.e. `simple-json`, `argonaut-codec`, `codec-argonaut`):
- Error messages:
  - show me all failures, not just first one
  - what was the JSON that caused the failure?
  - why are path "hints" inside `JsonDecodeError`?
- Single library for
  - both versions of codecs:
    - class-based
    - value-based
  - both directions of codecs:
    - unidirectional (encode/decode can be different)
    - bidirectional (encode/decode in same codec)
  - both 'styles' of JSON
    - idiomatic JSON
    - 'tagged' JSON
  - for migrating JSON
- Minimize imports by overloading each file
- Don't require me to copy code and make adjustments

## Design Goals

- better error messages when decoding Json
  - what caused the error?
  - give more control to user as to what counts as the error message
  - to support this, I need the user to easily define whether an error is thrown when decoding something (like, could just throw a `mempty :: Doc Void`)
- provide all errors when parsing, not just the first one
  - `V` and some monoid
  - In work code, I used `Doc Void` because it was a layout-preserving text value
  - and printing the error was nice
- Support both 'tagged' JSON and more idiomatic JSON
- Support both class- and value-based codecs
- Support both bidirectional and unidirectional codecs
- Support migration
- Minimize imports


## Error type

Analyzing error type:

```purs
data JsonDecodeError
  -- | Expected type A but got some other type that isn't specified
  = TypeMismatch String
  | --  | Catch all "got something other than what we were trying to decode" |
  | --- | ------------------------------------------------------------------ |Decoded the 'tag' to determine the contructor but it wasn't one of this type's constructors
  | --  | - Decoded a `Void` which is impossible |
  | --- | -------------------------------------- |Decoded a "collection" type and something failed in it
  | UnexpectedValue Json
  -- | Expected a field in object but it was missing`
  | MissingValue
  -- | Hint - Path: where in the JSON some error occurred
  | AtIndex Int JsonDecodeError
  -- | Hint - Path: where in the JSON some error occurred
  | AtKey String JsonDecodeError
  -- | Hint - Intent: that to which we were trying to decode
  | Named String JsonDecodeError
```

What kinds of errors can a user provide?
- straight-to-text errors: user doesn't care and crafts a custom message while decoding
- analysis errors: user is performing some analysis (how many X errors did we get?), but that can be done via WriterT
- structured errors
  - 3 lines: header, main, footer
  - 3x3 grid
- configured errors
  - pass that information in via the input record?


Leafs: only place where actual JSON decoding occurs where we need to know exactly what the error type should be
Branches: we shouldn't do any error handling here. Rather, it should be delegated to the user who calls `lmap` or something before this point.

When decoding (happy path)
  producing the things we care about
  ```
  foo <- decodeSomething
  bar <- tryOne <|> tryTwo
  ```

when error handling (unhappy path)
  defer to leaf handlers
  ```
  foo <- decodeInt
  ```
  or customize error message separately from the decoding
  ```
  bar <- decodeX `lmapFlipped` \e ->
    some custom error thing here
  ```


## Justifying the Design

### Error type

To help locate where the error occurred in the JSON, we need to keep track of where we are in the JSON. This is done via an `Array JsonOffset` argument that gets modified top-down whenever we decode an indexed value.

Since our code already uses a `Json -> Either err a`-like type (i.e. a `Function`), we can use two encodings:
- `Json -> Array JsonOffset -> Either err a`
- `ReaderT { json :: Json, pathSoFar :: Array JsonOffset } (Either err) a`

We chose the latter one because it reduces the number of curried functions. Not sure if this is a better idea, but thought I'd try it.

At its most primitive definition, the error type only allows 3 kinds of errors and forms a Tree-like structure:
- (leaf) primitive JSON decoding failure
    - type mismatch
    - missing object field
    - missing array index
- (leaf) logic error
    - decoded void
    - refinement error (Number -> Int, Array -> NonEmptyArray)
- (branch) grouping construct

The leaves can only be one thing. However, a branch can be a couple of things:
- just a grouping mechanism (i.e. nothing else is added at this point)
- a group with some context associated with it
- an error that removes the previous 'leaves' and summarizes them in a new leaf-like error. However, to create this error, we need the leaf errors.

```purs
newtype TreelikeError leafError = TreelikeError (Either (GroupError leafError) leafError)
  deriving (Functor, Foldable, Traversable)

-- data GroupError context leafError =
--   = Group (NonEmptyArray leafError)
--   | Group2 context (NonEmptyArray error)
--   | Group3 context

newtype GroupError' leafError = GroupError' (These Context (NonEmptyArray (TreelikeError leafError)))

-- with newtypes
GroupError' $ Both "Context" 
  [ TreelikeError $ Right leafError
  , TreelikeError $ Left $ Both "Context2"
      [ TreelikeError $ Right leafError2 ]
  , TreelikeError $ Left $ This "Context3"
  , TreelikeError $ Left $ That
      [ TreelikeError $ Right leafError2 ]
  ]

-- without newtypes
Both "Context" 
  [ Right leafError
  , Left $ Both "Context2"
      [ Right leafError2 ]
  , Left $ This "Context3"
  , Left $ That
      [ Right leafError2 ]
  ]
```

## Error Accumulation

Goals:
  - Can use `a <> b` to run same JSON input across two functions and `<>` their results
  - Can use `a <|> b` to run same JSON input across two functions and `<|>` their results where
    - `a' <> b'` has the effect of recordin

## Goal: keep `err` polymorphic and still define 1 type class

Possible error types:
- `TreelikeError`
- `NonEmptyArray`
- `Dodo a`

1. encode error as `TreelikeError` and provide a function that maps `TreelikeError` to the `err`
   1. downsides: paying the cost of intermediate data structures...
2. define another type class `IsJsonDecodeError` that knows how to handle all errors? / defines all error types, so we can reuse the functions from that type?
   1. not sure if this is always desirable...

Other idea: use a polymorphic type `f PrimitiveJsonError a` that allows me to reuse `TreelikeError` (e.g. `Compose Const TreelikeError) but allows caller to override the type with something else.

## Context

- feels like there needs to be a bottom-up propagation of information:
  - when reusing some other type, don't show its type hint. Rather, we should only show the type hint when we're actually decoding that value.
  - Some context seems like it should "merge"
    - add type hint
      - add constructor hint
        - add subterm hint
    - 1 error: "Type.Ctor's subterm x
    - nested error
      - When decoding Type
        - Ctor
          - subterm x
            - error...
- Idea: use a `WriterT (List DecodingHint)`?
  - addHint
  - need some way to say
    - when decoding this type as is, don't censor its hints
    - when using this decoder to decode larger type, censor its hints

data DecodingHint
  = TypeName String
  | CtorName String
  | SubtermIndex Int
  | Label String

## Error message

```
Could not convert Number to Int: 20420204204024024242402.42
  at path Z

having failed to decode Type
  at path Z
  due to error: 

having failed to decode Type
  at path Z
  due to error: 

while decoding the label "foo"
  at path Z
while decoding the subterm X
  at path Z
while decoding the constructor Ctor
  at path Z
while decoding Type
  at path Z
```

## Next Actions

- reimplement the `<|>` instance for `JsonDecoder`, so that its implementation uses a new member from the `IsDecodeJsonError`
- add new member to type class that defines
    - how to combine errors in a way that's different from just `<>`
    - (Maybe my types are just wrong?)
- add 'decodeOptionalProp/s'
  - update the main encoder to use a function, so can use `identity` if no field should be added
