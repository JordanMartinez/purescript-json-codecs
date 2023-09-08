// @inline export encoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dArray from "../Data.Array/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dOrd from "../Data.Ord/index.js";
import * as Data$dOrdering from "../Data.Ordering/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const encoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayNil)({
    reflectSymbol: () => "zAppearsFirst"
  })()())({reflectSymbol: () => "reqRen"})()())({reflectSymbol: () => "req"})()())({reflectSymbol: () => "optRen"})()())({reflectSymbol: () => "optArr"})()())({
    reflectSymbol: () => "opt"
  })()())({reflectSymbol: () => "nested"})()();
  const $1 = {
    req: x => Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(x))}),
    reqRen: x => Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.$Maybe("Just", "otherName"), insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromString(x)}),
    zAppearsFirst: x => Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.$Maybe("Just", 1), value: Data$dArgonaut$dCore.fromString(x)}),
    opt: v1 => {
      if (v1.tag === "Just") { return Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromString(v1._1)}); }
      return Data$dMaybe.Nothing;
    },
    optRen: v1 => {
      if (v1.tag === "Just") {
        return Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.$Maybe("Just", "otherName2"), insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromString(v1._1)});
      }
      return Data$dMaybe.Nothing;
    },
    optArr: arr => {
      if (arr.length === 0) { return Data$dMaybe.Nothing; }
      return Data$dMaybe.$Maybe(
        "Just",
        {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(arr))}
      );
    },
    nested: (() => {
      const $1 = Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayCons(Codec$dJson$dUnidirectional$dValue.fromRecordPropArrayNil)({
        reflectSymbol: () => "other"
      })()())({reflectSymbol: () => "foo"})()();
      const $2 = {
        other: x => Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromBoolean(x)}),
        foo: v1 => {
          if (v1.tag === "Just") {
            return Data$dMaybe.$Maybe("Just", {key: Data$dMaybe.Nothing, insertionOrder: Data$dMaybe.Nothing, value: Data$dArgonaut$dCore.fromBoolean(v1._1)});
          }
          return Data$dMaybe.Nothing;
        }
      };
      return x => Data$dMaybe.$Maybe(
        "Just",
        {
          key: Data$dMaybe.Nothing,
          insertionOrder: Data$dMaybe.Nothing,
          value: Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => r => {
            const $3 = r.key;
            const $4 = r.value;
            return Foreign$dObject.mutate($5 => () => {
              $5[$3] = $4;
              return $5;
            })(acc);
          })(Foreign$dObject.empty)(Data$dArray.sortBy(l => r => {
            const $3 = Data$dOrd.ordInt.compare(l.insertionOrder)(r.insertionOrder);
            const $4 = Data$dOrd.ordString.compare(l.key)(r.key);
            if ($3 === "LT") { return Data$dOrdering.LT; }
            if ($3 === "GT") { return Data$dOrdering.GT; }
            if ($3 === "EQ") { return $4; }
            $runtime.fail();
          })($1.fromRecordPropArray(Type$dProxy.Proxy)($2)(x))))
        }
      );
    })()
  };
  return values => Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => r => {
    const $2 = r.key;
    const $3 = r.value;
    return Foreign$dObject.mutate($4 => () => {
      $4[$2] = $3;
      return $4;
    })(acc);
  })(Foreign$dObject.empty)(Data$dArray.sortBy(l => r => {
    const $2 = Data$dOrd.ordInt.compare(l.insertionOrder)(r.insertionOrder);
    const $3 = Data$dOrd.ordString.compare(l.key)(r.key);
    if ($2 === "LT") { return Data$dOrdering.LT; }
    if ($2 === "GT") { return Data$dOrdering.GT; }
    if ($2 === "EQ") { return $3; }
    $runtime.fail();
  })($0.fromRecordPropArray(Type$dProxy.Proxy)($1)(values))));
})();
const test = j => Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => r => {
  const $0 = r.key;
  const $1 = r.value;
  return Foreign$dObject.mutate($2 => () => {
    $2[$0] = $1;
    return $2;
  })(acc);
})(Foreign$dObject.empty)(Data$dArray.sortBy(l => r => {
  const $0 = Data$dOrd.ordInt.compare(l.insertionOrder)(r.insertionOrder);
  const $1 = Data$dOrd.ordString.compare(l.key)(r.key);
  if ($0 === "LT") { return Data$dOrdering.LT; }
  if ($0 === "GT") { return Data$dOrdering.GT; }
  if ($0 === "EQ") { return $1; }
  $runtime.fail();
})((() => {
  const arr = [
    {key: "req", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(j.req))},
    {key: "otherName", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromString(j.reqRen)},
    {key: "zAppearsFirst", insertionOrder: 1, value: Data$dArgonaut$dCore.fromString(j.zAppearsFirst)}
  ];
  return [
    {
      key: "nested",
      insertionOrder: 2147483647,
      value: Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => r => {
        const $0 = r.key;
        const $1 = r.value;
        return Foreign$dObject.mutate($2 => () => {
          $2[$0] = $1;
          return $2;
        })(acc);
      })(Foreign$dObject.empty)(Data$dArray.sortBy(l => r => {
        const $0 = Data$dOrd.ordInt.compare(l.insertionOrder)(r.insertionOrder);
        const $1 = Data$dOrd.ordString.compare(l.key)(r.key);
        if ($0 === "LT") { return Data$dOrdering.LT; }
        if ($0 === "GT") { return Data$dOrdering.GT; }
        if ($0 === "EQ") { return $1; }
        $runtime.fail();
      })((() => {
        const arr$1 = [{key: "other", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromBoolean(j.nested.other)}];
        if (j.nested.foo.tag === "Just") { return [{key: "foo", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1)}, ...arr$1]; }
        return arr$1;
      })())))
    },
    ...(() => {
      const arr$1 = j.optRen.tag === "Just" ? [{key: "otherName2", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromString(j.optRen._1)}, ...arr] : arr;
      const arr$2 = j.optArr.length === 0
        ? arr$1
        : [{key: "optArr", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(j.optArr))}, ...arr$1];
      if (j.opt.tag === "Just") { return [{key: "opt", insertionOrder: 2147483647, value: Data$dArgonaut$dCore.fromString(j.opt._1)}, ...arr$2]; }
      return arr$2;
    })()
  ];
})())));
export {encoder, test};
