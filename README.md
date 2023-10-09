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
  - I don't want to use your "does not allow custom error messages" type, `JsonDecodeError`
  - Allow me to specify the order in which to encode something
  - Allow me the ability to rename fields when encoding/decoding

Improve error messages:
- on type mismatch errors, show me what you expected AND got, not just what was expected
- allow me to write custom error messages

Provide record syntax that distinguishes between
  - when decoding
    - optional fields: whether a field is required or optional
    - renaming: decoding a key "foo" but putting it in a record label "bar"
  - when encoding
    - order of fields: the order in which to encode fields
    - renaming: encoding a record label "bar" but encoding it as "foo" in the JSON
  - See [snapshots](./snapshots/Snapshot/)

For an example of decoding text and encoding examples, see [./test/test-output.txt](./test/test-output.txt)

## License

- [`codec`](https://github.com/garyb/purescript-codec) - Copyright (c) 2019 Gary Burgess (MIT)
- [`codec-argonaut`](https://github.com/garyb/purescript-codec-argonaut/) - Copyright (c) 2017 Gary Burgess (MIT)
- [`argonaut-codec`](https://github.com/purescript-contrib/purescript-argonaut-codecs) - Copyright (c) 2020 PureScript Contrib (MIT)
