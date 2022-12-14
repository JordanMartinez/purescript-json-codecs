# purescript-json-codecs

A bidirectional and unidirectional value-based and runtime-configurable typeclass-based JSON codec library. Essentially, [`argonaut-codec`](https://pursuit.purescript.org/packages/purescript-argonaut-codecs) and [`codec-argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut) in one library with clear imports, flexible error types, error accumulation, and better performance (claim unproven but hopefully true).

## Design

See [DESIGN.md](DESIGN.md) for the reasoning behind this library's design. The rest of this section covers my goals and where I'm at in that process.

Address gripes with other JSON codec libraries:
- [`simple-json`](https://pursuit.purescript.org/packages/purescript-simple-json/):
  - I don't want to have to [fork or copy the repo](https://github.com/justinwoo/purescript-simple-json#how-should-i-actually-use-this-library) to benefit from it / change various type class instances.
  - I don't want to use your information-poor error type, `NonEmptyList ForeignError`
- [`argonaut-codec`](https://pursuit.purescript.org/packages/purescript-argonaut-codecs)/[`codec-argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut):
  - I don't want to use your information-poor error type, `JsonDecodeError`
  - I want one library for value-based or typeclass-based codecs

Improve error messages:
- show me all failures, not just the first one (i.e. use `V`, not `Either`)
- show me hints separate from the error message
- on type mismatch errors, show me what you expected AND got, not just what was expected
- give me more control over what my error type is

Provide a single library for both versions of codecs and both directions of codecs
  - [x] unidirectional (one can implement only encode/decode, and they can be different)
    - [x] class-based
      - [x] configurable decoder
      - [x] configurable encoder
    - [x] value-based
  - [x] bidirectional (encode/decode must be bidirectional)
     - [x] class-based
      - [x] configurable decoder
      - [x] configurable encoder
    - [x] value-based

Provide record syntax I enjoy using
  - See examples below

## Error Messages for `test/Main.purs`

### `PrimitiveJsonError`:

```
Decode Int to Int:
1

Decode Int to String:
while decoding the type, Int, at path: ROOT
Expected number but got string: "foo" at path: ROOT


Decode Record to Int:
while decoding the type, Int, at path: ROOT
Expected number but got object with 7 keys at path: ROOT


Decode Record incorrectly:
while decoding the type, Record, at path: ROOT
  Expected number but got string: "hello" at path: ROOT.string

  while decoding the type, Array, at path: ROOT.record
  Expected array but got object with 2 keys at path: ROOT.record

  while decoding the type, Int, at path: ROOT.object
  Expected number but got object with 4 keys at path: ROOT.object

  Expected boolean but got number: 1.4 at path: ROOT.number

  Expected string but got number: 9.0 at path: ROOT.int

  Expected string but got boolean: true at path: ROOT.boolean

  while decoding the type, Object, at path: ROOT.array
  Expected object but got array of length 10 at path: ROOT.array
```

### `Doc Void`

```
Decode Int to Int:
1

Decode Int to String:
while decoding the type, Int, at path: ROOT
  Expected number but got string: "foo" at path: ROOT

Decode Record to Int:
while decoding the type, Int, at path: ROOT
  Expected number but got object with 7 keys at path: ROOT

Decode Record incorrectly:
while decoding the type, Record, at path: ROOT
  Expected number but got string: "hello" at path: ROOT.string

  while decoding the type, Array, at path: ROOT.record
    Expected array but got object with 2 keys at path: ROOT.record

  while decoding the type, Int, at path: ROOT.object
    Expected number but got object with 4 keys at path: ROOT.object

  Expected boolean but got number: 1.4 at path: ROOT.number

  Expected string but got number: 9.0 at path: ROOT.int

  Expected string but got boolean: true at path: ROOT.boolean

  while decoding the type, Object, at path: ROOT.array
    Expected object but got array of length 10 at path: ROOT.array
```

### `Doc GraphicsParam`

Same as `Doc Void` but with colors:

![Same as Doc Void but with colors](assets/doc-graphics-param-error.png)

## Codec Examples

### Unidirectional Value-based codec where all fields are required

```purs
-- Error messages are outputted with color.
fooDecoder :: JsonDecoder (Doc GraphicsParam) _
fooDecoder =
  decodeRecord
    { a: adeD $ decodeRecord
        { foo: adeD $ decodeBoolean }
    , b: adeD $ decodeEither decodeInt $ decodeArray decodeString
    , c: adeD $ decodeMaybeTagged decodeString
    }

fooEncoder :: _ -> Json
fooEncoder =
  encodeRecord
    { a: encodeRecord
        { foo: encodeBoolean }
    , b: encodeEither encodeInt $ encodeArray encodeString
    , c: encodeMaybeTagged encodeString
    }
```

### Unidirectional Value-based codec where some fields are optional

```purs
{-
This decodes to
  { a :: { foo :: Boolean }
  , b :: Either Int (Array String)
  , c :: Maybe String
  , optionalA :: Maybe Int    -- decodes to Nothing if field is missing.
  , optionalB :: Maybe String -- decodes to Nothing if field is missing.
  }
-}
barDecoder :: JsonDecoder (Doc GraphicsParam) _
barDecoder =
  decodeRecordPrim $ buildRecordDecoder $
    decodeRequiredProps
      { a: adeD $ decodeRecord
          { foo: adeD $ decodeBoolean }
      , b: adeD $ decodeEither decodeInt $ decodeArray decodeString
      , c: adeD $ decodeMaybeTagged decodeString
      }
    >>> decodeOptionalProps
      { optionalA: adeD decodeInt
      , optionalB: adeD decodeString
      }

barEncoder :: Jsonencoder (Doc GraphicsParam) _
barEncoder =
  encodeRecordPrim $ buildRecordencoder $
    encodeRequiredProps
      { a: encodeRecord
          { foo: encodeBoolean }
      , b: encodeEither encodeInt $ encodeArray encodeString
      , c: encodeMaybeTagged encodeString
      }
    >>> encodeOptionalProps
      { optionalA: encodeInt
      , optionalB: encodeString
      }
```

### Unidirectional Typeclass-based codec where some fields are optional

```purs
type Baz =
  { a :: { foo :: Boolean }
  , b :: Either Int (Array String)
  , c :: Maybe String                    -- required field using tagged Maybe approach
  , optionalA :: Optional (Maybe Int)    -- decodes to Nothing if field is missing.
  , optionalB :: Optional (Maybe String) -- decodes to Nothing if field is missing.
  }

bazDecoder :: JsonDecoder (Doc GraphicsParam) Baz
bazDecoder = decodeJson

bazEncoder :: Baz -> Json
bazEncoder = encodeJson
```

### Unidirectional Typeclass-based codec where some instances are implemented via runtime-configured implementations

See [Decoding.purs](./test/Test/Codec/Json/Unidirectional/Typeclass/LocalOverrides/Decoding.purs), which as of the time of writing, outputs:

```
### Typeclass (Local Overrides) Output:
Starting record: 
{ stringNormal: "hello"
, stringOverride: "+"
, tupleNormal: (Tuple "left" "right")
, tupleSwap: (Tuple "left" "right")
, tupleMultiOverride: (Tuple "left" "+")
}
Encoded Json: 
{
  "tupleSwap": [
    "left",
    "right"
  ],
  "tupleNormal": [
    "left",
    "right"
  ],
  "tupleMultiOverride": [
    "left",
    "+"
  ],
  "stringOverride": "+",
  "stringNormal": "hello"
}
Normal decoded value: 
{ stringNormal: "hello"
, stringOverride: "+"
, tupleNormal: (Tuple "left" "right")
, tupleSwap: (Tuple "left" "right")
, tupleMultiOverride: (Tuple "left" "+")
}
Locally-overridden decoded value: 
{ stringNormal: "hello"
, stringOverride: "1 + 1 == 2"
, tupleNormal: (Tuple "left" "right")
, tupleSwap: (Tuple "right" "left")
, tupleMultiOverride: (Tuple "1 + 1 == 2" "left")
}
```

## Bidirectional Value-based Decoding and Encoding

See [test/Test/Codec/Json/Bidirectional/AnsiDodoError.purs](./test/Test/Codec/Json/Bidirectional/AnsiDodoError.purs)

## License

- [`codec`](https://github.com/garyb/purescript-codec) - Copyright (c) 2019 Gary Burgess (MIT)
- [`codec-argonaut`](https://github.com/garyb/purescript-codec-argonaut/) - Copyright (c) 2017 Gary Burgess (MIT)
- [`argonaut-codec`](https://github.com/purescript-contrib/purescript-argonaut-codecs) - Copyright (c) 2020 PureScript Contrib (MIT)
