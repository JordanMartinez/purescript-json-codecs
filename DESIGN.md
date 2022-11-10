# Design

## Start

```pur
Json -> Either JsonDecoderErrror a
```

## Converting to a `ReaderT`-based representation

```pur
ReaderT Json (Either JsonDecoderErrror) a
```

## Error type

Analyzing the `argonaut-codec`/`codec-argonaut` error type:

```purs
data JsonDecodeError
  -- | Expected type A but got some other type that isn't specified
  = TypeMismatch String

  | --  | Catch all "got something other than what we were trying to decode" |
  | --- | ------------------------------------------------------------------ |Decoded the 'tag' to determine the contructor but it wasn't one of this type's constructors
  | --  | - Decoded a `Void` which is impossible |
  | --- | -------------------------------------- |Decoded a "collection" type and something failed in it
  | UnexpectedValue Json
  -- | Expected a field in object but it was missing.
  | MissingValue
  {- Note: this constructor doesn't exist
  -- | Expected an index in array but it was missing.
  | MissingIndex                                                                    -}
  -- | Path hint: where in the JSON some error occurred
  | AtIndex Int JsonDecodeError
  -- | Path hint: where in the JSON some error occurred
  | AtKey String JsonDecodeError
  -- | Type hint: that to which we were trying to decode the JSON
  | Named String JsonDecodeError
```

The above analysis shows two kinds of errors:
- Leafs: a place where decoding JSON actually failed
- Branches: context that helps us understand the leaf errors.

## A New Error type

To help locate where the error occurred in the JSON, we need to keep track of where we are in the JSON. This is done via an `Array JsonOffset` argument that gets modified top-down whenever we decode an indexed value.

```diff
-ReaderT {                                json :: Json } (Either err) a
+ReaderT { pathSoFar :: Array JsonOffset, json :: Json } (Either err) a
```

We chose the latter one because it reduces the number of curried functions. Not sure if this is a better idea, but thought I'd try it.

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

## Error Accumulation

Using `Either` doesn't accumulate errors. So, the first failure will stop the decoding and immediately exit. Using `V` will accumulate the errors. So, we get as much information we can about all failures before stopping the decoding and exiting.

Thus, we go from:
```diff
-                 ReaderT { pathSoFar :: Array JsonOffset, json :: Json } (Either err) a
+Semigroup err => ReaderT { pathSoFar :: Array JsonOffset, json :: Json } (V      err) a
```

`V` does not have an `Alt` instance (e.g. `a <|> b`) because it's not clear whether the user wants the errors to accumulate or not. For example, using `Either err a` where `err` has a `Semigroup` instance:
```
-- Same as just `l`, so no reason to define this helper function...
altKeepFirstError :: Either err a -> Either err a -> Either err a
altKeepFirstError l _ = case l of
  Left _ -> l
  Right a -> l

-- Ignore previous errors and use the last one
altKeepLastError :: Either err a -> Either err a -> Either err a
altKeepLastError l r = case l of
  Left _ -> r
  Right a -> l

-- Store all errors
altAccumulate :: Either err a -> Either err a -> Either err a
altAccumulate l r = case l of
  Left e1 -> case r of
    Left e2 -> Left $ e1 <> e2
    Right a -> r
  Right a -> l
```

## Minimizing `Proxy` arguments

Throughout this codebase, there are newtypes like `RLRecord` or `RowListObject`. These newtypes remove the need to pass `Proxy` args to a type class function to help guide type inference. Rather, that information is passed via the newtype but uses one less curried function to do so.

## Minimizing Boilerplate for Runtime-Configured Typeclass Instances

See the ideas explained in https://github.com/JordanMartinez/local-typeclass-instances

This gives us the following type:

```diff
Semigroup err =>
  ReaderT
    { pathSoFar :: Array JsonOffset
+   , extra :: extra
    , json :: Json
    }
    (V err)
    a
```



## Enabling custom error types

```diff
Semigroup err
=> ReaderT 
    { pathSoFar :: Array JsonOffset
+   , handlers :: JsonErrorHandlers err
    , extra :: extra
    , json :: Json
    }
    (V err)
    a
```

## Inlining the Semigroup instance to reduce curried functions

```diff
-Semigroup err
-=> ReaderT 
+  ReaderT 
    { json :: Json
    , pathSoFar :: Array JsonOffset
+   , appendFn :: e -> e -> e
    , handlers :: JsonErrorHandlers err
    , extra :: extra
    }
    (V err)
    a
```

## Using uncurried functions for performance

```diff
-ReaderT { pathSoFar :: Array JsonOffset, handlers :: JsonErrorHandlers err, extra :: extra, json :: Json } (V err) a
+Fn3                   (Array JsonOffset)             JsonErrorHandlers err           extra          Json   (V err  a)
```

