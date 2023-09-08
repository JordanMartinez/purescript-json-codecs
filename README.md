# purescript-json-codecs

A unidirectional value-based JSON codec library.

Whether this is more performant than other options is still being clarified. See the [benchmarks](./bench).

## Design

See [DESIGN.md](DESIGN.md) for the reasoning behind this library's design. The rest of this section covers my goals and where I'm at in that process.

Address gripes with other JSON codec libraries:
- [`simple-json`](https://pursuit.purescript.org/packages/purescript-simple-json/):
  - I don't want to have to [fork or copy the repo](https://github.com/justinwoo/purescript-simple-json#how-should-i-actually-use-this-library) to benefit from it / change various type class instances.
  - I don't want to use your information-poor error type, `NonEmptyList ForeignError`
- [`argonaut-codec`](https://pursuit.purescript.org/packages/purescript-argonaut-codecs)/[`codec-argonaut`](https://pursuit.purescript.org/packages/purescript-codec-argonaut):
  - I don't want to use your information-poor error type, `JsonDecodeError`

Improve error messages:
- show me all failures, not just the first one
- on type mismatch errors, show me what you expected AND got, not just what was expected
- allow me to write custom error messages

Provide record syntax I enjoy using
  - See [snapshots](./snapshots/Snapshot/)

## Sample Json decode message:

Given the following code...
```purs
toNel :: forall a. Array a -> NonEmptyList a
toNel = unsafePartial fromJust <<< NEL.fromList <<< List.fromFoldable

printDecodeError
    $ AtKey "some-key"
    $ AtIndex 0
    $ AccumulateError $ toNel
      [ AccumulateError $ toNel [ AtIndex 0 $ DecodeError "Error0" ]
      , AtKey "bar" $ AtIndex 1 $ DecodeError "Error1"
      , AtKey "baz" $ accumulateErrors
          (AtIndex 0 $ DecodeError "Error2")
          (AtIndex 1 $ DecodeError "Error3")
      , AccumulateError $ toNel
          [ AccumulateError $ toNel [ AtIndex 1 $ DecodeError "Error4" ]
          , AtKey "last1" $ DecodeError "Error5"
          , AtKey "last2" $ DecodeError "Error6"
          ]
      , AccumulateError $ toNel
          [ AtKey "last3" $ DecodeError "Error7"
          , AtKey "last4" $ DecodeError "Error8"
          , AccumulateError $ toNel [ AtIndex 2 $ DecodeError "Error9" ]
          ]
      , AccumulateError $ toNel
          [ AccumulateError $ toNel
              [ AtKey "a" $ DecodeError "Error10"
              , AtKey "b" $ DecodeError "Error11"
              , AtKey "c" $ DecodeError "Error12"
              ]
          , AccumulateError $ toNel
              [ AtKey "x" $ DecodeError "Error13"
              , AtKey "y" $ DecodeError "Error14"
              , AtKey "z" $ DecodeError "Error15"
              ]
          ]
      ]
```

the error message printed is the following. Indented values are all of the errors that happened under the given JSON path:
```
ROOT."some-key"[0]
  [0] - Error0
  ."bar"[1] - Error1
  ."baz"
    [1] - Error3
    [0] - Error2
  [1] - Error4
  ."last1" - Error5
  ."last2" - Error6
  ."last3" - Error7
  ."last4" - Error8
  [2] - Error9
  ."a" - Error10
  ."b" - Error11
  ."c" - Error12
  ."x" - Error13
  ."y" - Error14
  ."z" - Error15
```

## License

- [`codec`](https://github.com/garyb/purescript-codec) - Copyright (c) 2019 Gary Burgess (MIT)
- [`codec-argonaut`](https://github.com/garyb/purescript-codec-argonaut/) - Copyright (c) 2017 Gary Burgess (MIT)
- [`argonaut-codec`](https://github.com/purescript-contrib/purescript-argonaut-codecs) - Copyright (c) 2020 PureScript Contrib (MIT)
