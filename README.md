# purescript-json-codecs

A JSON codec library that supports value-based and configurable typeclass-based json codecs

## Design

Address gripes with other JSON codec libraries:
- `simple-json`:
  - I don't want to have to fork the repo to benefit from it / change various type class instances.
  - I don't want to use your poor error type, `NonEmptyList ForeignError`
- `argonaut-codec`/`codec-argonaut`:
  - I don't want to use your poor error type, `JsonDecodeError`
  - I want one library for value-based or typeclass-based codecs

Improve error messages:
- show me all failures, not just the first one (i.e. use `V`, not `Either`)
- show me hints separate from the error message
- on type mismatch errors, show me what you expected AND got, not just what was expected
- give me more control over what my error type is

Provide a single library for
  - both versions of codecs:
    - [x] class-based
    - [x] value-based
  - both directions of codecs:
    - [x] unidirectional (one can implement only encode/decode, and they can be different)
    - [ ] bidirectional (encode/decode must be bidirectional) - **WIP**
  - [ ] for migrating JSON using old/outdated codecs easily - **WIP**

Provide record syntax I enjoy using
  - See [the test folder's `Main.purs` for an example](./test/Main.purs).

Make it easy to decode optional fields in records.
  - [ ] **WIP** automatic removal of utility newtypes
