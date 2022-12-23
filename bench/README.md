# Benchmarks

The JSON files are rendered via [purescript-benchotron-svg-renderer (Jordan's fork)](https://github.com/jordanmartinez/purescript-benchotron-svg-renderer).

Each non-`Util` file in [`Codec/Benchmarks`](./Codec/Benchmarks/) contains a benchmark whose results are stored in the [results](./results) folder sharing a similar name.

Benchmarks can be run via

```sh
npm i
npm i -g purs-backend-es
npm run bench
```
