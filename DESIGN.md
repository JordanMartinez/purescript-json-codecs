# Design

## Start

```pur
Json -> Either JsonDecoderErrror a
```

## Converting to a `ReaderT`-based representation

```pur
ReaderT Json (Either JsonDecoderErrror) a
```

## Redefining the Error type

### Analyzing the Current Error Type: `JsonDecodeError`

Analyzing the `argonaut-codec`/`codec-argonaut` error type:

```purs
data JsonDecodeError
  -- Expected type A but got some other type that isn't specified
  = TypeMismatch String

  -- Catch all "got something other than what we were trying to decode":
  -- - Decoded the 'tag' to determine the contructor but it wasn't one of this type's constructors
  -- - Decoded a `Void` which is impossible |
  -- - Decoded a "collection" type and something failed in it
  | UnexpectedValue Json
  -- Expected a field in object but it was missing.
  | MissingValue
  {- Note: this constructor doesn't exist
  -- Expected an index in array but it was missing.
  | MissingIndex                                                                    -}
  -- Path hint: where in the JSON some error occurred
  | AtIndex Int JsonDecodeError
  -- Path hint: where in the JSON some error occurred
  | AtKey String JsonDecodeError
  -- Type hint: that to which we were trying to decode the JSON
  | Named String JsonDecodeError
```

The above analysis shows two kinds of errors:
- Leafs: a place where decoding JSON actually failed
- Branches: context that helps us understand the leaf errors.

In addition, the path into the JSON is also stored in multiple places throughout the error.

### A New Error type

To help locate where the error occurred in the JSON, we need to keep track of where we are in the JSON. This is done via an `Array JsonOffset` argument that gets modified top-down whenever we decode an indexed value.

```diff
-ReaderT {                                json :: Json } (Either err) a
+ReaderT { pathSoFar :: Array JsonOffset, json :: Json } (Either err) a
```

At its most primitive definition, the error type only allows 3 kinds of errors and forms a Tree-like structure:
- (leaf) primitive JSON decoding failure
    - type mismatch
    - missing object field
    - missing array index
- (leaf) logic error
    - decoded void
    - refinement error (Number -> Int, Array -> NonEmptyArray)
    - structure error (e.g. expected a `tag` field to have `"Just"` or `"Nothing"` but got some other value when decoding a `Maybe`)
- (branch) grouping construct

While the leaves can only be one thing, a branch can be a couple of things:
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

-- Or to simplify it even futher into one type:
-- - Left = branch
-- - Right = leaf
newtype TreeError context leafError =
  TreeError (Either (These context (NonEmptyArray (TreeError context leafError))) leafError)

-- with newtypes
TreeError $ Left $ Both "Context" 
  [ TreeError $ Right leafError
  , TreeError $ Left $ Both "Context2"
      [ TreeError $ Right leafError2 ]
  , TreeError $ Left $ This "Context3"
  , TreeError $ Left $ That
      [ TreeError $ Right leafError2 ]
  ]

-- without newtypes
Left $ Both "Context" 
  [ Right leafError
  , Left $ Both "Context2"
      [ Right leafError2 ]
  , Left $ This "Context3"
  , Left $ That
      [ Right leafError2 ]
  ]
```

### Enabling custom errors via `JsonErrorHandlers`

While we could use the above error type, enabling the user to define their own is more flexible. Using the original `JsonDecodeError` as inspiration, we define the following handlers for an API for defining custom errors:
- a handler for producing an error when decoding a primitive JSON value fails (i.e. `null`, `Number`, `String`, `Array Json`, `Object Json`): `onTypeMismatch`
- a handler for when we expected a field or index within an object or array to exist, and it didn't: `onMissingField`/`onMissingIndex`
- a handler for when a primitive JSON value was properly decoded, but it couldn't be further refined (e.g. `String -> NonEmptyString`): `onUnrefinableValue`
- a handler for when the JSON should have an expected structure or shape and it didn't (e.g. an object's `tag` value should be `"Just"` or `"Nothing"`): `onStructureError`
- a handler for modifying an existing error with additional hints or context (e.g. which type we were trying to decode): `addHint`
- whether we want to update the Json offset path (i.e. `Array JsonOffset`) every time we add the corresponding `addHint (AtKey someKey` or `addHint (AtIndex someIndex`) hints): `includeJsonOffset`. When this isn't needed, we disable this feature.

While we could define these functions using curried functions, they will generally only be defined once and used in many places, so we'll also use uncurried functions here, too.

```purs
newtype JsonErrorHandlers e = JsonErrorHandlers
  -- Array JsonOffset -> ExpectedJsonType -> ActualJsonType -> e
  { onTypeMismatch :: Fn3 (Array JsonOffset) ExpectedJsonType ActualJsonType e
  -- Array JsonOffset -> String -> e
  , onMissingField :: Fn2 (Array JsonOffset) String e
  -- Array JsonOffset -> Int -> e
  , onMissingIndex :: Fn2 (Array JsonOffset) Int e
  -- Array JsonOffset -> String -> e
  , onUnrefinableValue :: Fn2 (Array JsonOffset) String e
  -- Array JsonOffset -> String -> e
  , onStructureError :: Fn2 (Array JsonOffset) String e
  , includeJsonOffset :: Boolean
  -- Array JsonOffset -> TypeHint -> e -> e
  , addHint :: Fn3 (Array JsonOffset) TypeHint e e
  }
```

This gets us:
```diff
- ReaderT { pathSoFar :: Array JsonOffset,                                   json :: Json } (Either err) a
+ ReaderT { pathSoFar :: Array JsonOffset, handlers :: JsonErrorHandler err, json :: Json } (Either err) a
```

## Adding Error Accumulation

### Switching from `Either` to `V`

Using `Either` doesn't accumulate errors. So, the first failure will stop the decoding and immediately exit. Using `V` will accumulate the errors. So, we get as much information we can about all failures before stopping the decoding and exiting.

Thus, we go from:
```diff
-                 ReaderT { pathSoFar :: Array JsonOffset, handlers :: JsonErrorHandler err, json :: Json } (Either err) a
+Semigroup err => ReaderT { pathSoFar :: Array JsonOffset, handlers :: JsonErrorHandler err, json :: Json } (V      err) a
```

`V` does not have an `Alt` instance (e.g. `a <|> b`) because it's not clear whether the user wants the errors to accumulate or not. For example, using `Either err a` where `err` has a `Semigroup` instance:
```
-- Same as just `l`, so no reason to define this helper function...
altKeepFirstError :: Either err a -> Either err a -> Either err a
altKeepFirstError l _ = case l of
  Right a -> l
  Left _ -> l

-- Ignore previous errors and use the last one
altKeepLastError :: Either err a -> Either err a -> Either err a
altKeepLastError l r = case l of
  Right a -> l
  Left _ -> r

-- Store all errors
altAccumulate :: Either err a -> Either err a -> Either err a
altAccumulate l r = case l of
  Right a -> l
  Left e1 -> case r of
    Left e2 -> Left $ e1 <> e2
    Right a -> r
```

### Inlining the `Semigroup` instance to reduce curried functions

```diff
-Semigroup err
-=> ReaderT 
+  ReaderT 
    { pathSoFar :: Array JsonOffset
+   , appendFn :: err -> err -> err
    , handlers :: JsonErrorHandlers err
    , json :: Json
    }
    (V err)
    a
```

## Making Typeclass-Based Codecs Runtime-Configurable

See the ideas explained in https://github.com/JordanMartinez/local-typeclass-instances

This gives us the following type:

```diff
  ReaderT
    { pathSoFar :: Array JsonOffset
    , appendFn :: err -> err -> err
    , handlers :: JsonErrorHandler err
+   , extra :: extra
    , json :: Json
    }
    (V err)
    a
```

## Striving for Performance by Using Uncurried Functions Everywhere

```diff
-ReaderT
- { pathSoFar :: Array JsonOffset
- , appendFn :: e -> e -> e
- , handlers :: JsonErrorHandlers err
- , extra :: extra, json :: Json 
- } 
- (V err) a
+Fn5
+ (Array JsonOffset)
+ (Fn2 e e e)
+ (JsonErrorHandlers err)         
+ extra
+ Json 
+ (V err  a)
```

Simplifying the `Fn5`, we get:
```purs
Fn5 (Array JsonOffset) (e -> e -> e) (JsonErrorHandlers err) extra Json (V err a)
```

which is the type for `DecoderFn`:
```pur
newtype DecoderFn path appendFn handlers extra from to =
  DecoderFn (Fn5 path appendFn handlers extra from to)

type JsonDecoder' e extra from to =
  DecoderFn (Array JsonOffset) (e -> e -> e) (JsonErrorHandlers e) extra from to
```

## Other library notes

### Minimizing `Proxy` arguments

Throughout this codebase, there are newtypes like `RLRecord` or `RowListObject` that are typically used in `Record` or `Variant`-related type clases. These newtypes remove the need to pass `Proxy` args to a type class function to help guide type inference. Rather, that information is passed via the newtype. This makes the code a bit harder to read because there is additional wrapping and unwrapping going on, but the result is one less curried function.
