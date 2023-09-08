// @inline export encoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const encoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjNil)({
    reflectSymbol: () => "reqRen"
  })()())({reflectSymbol: () => "req"})()())({reflectSymbol: () => "optRen"})()())({reflectSymbol: () => "optArr"})()())({reflectSymbol: () => "opt"})()())({
    reflectSymbol: () => "nested"
  })()();
  const $1 = {
    req: (() => {
      const $1 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      return x => Data$dMaybe.$Maybe("Just", $1(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(x))));
    })(),
    reqRen: (() => {
      const $1 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName"));
      return x => Data$dMaybe.$Maybe("Just", $1(Data$dArgonaut$dCore.fromString(x)));
    })(),
    opt: Data$dMaybe.functorMaybe.map((() => {
      const $1 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      return x => $1(Data$dArgonaut$dCore.fromString(x));
    })()),
    optRen: Data$dMaybe.functorMaybe.map((() => {
      const $1 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
      return x => $1(Data$dArgonaut$dCore.fromString(x));
    })()),
    optArr: arr => {
      if (arr.length === 0) { return Data$dMaybe.Nothing; }
      return Data$dMaybe.$Maybe("Just", Data$dTuple.$Tuple(Data$dMaybe.Nothing, Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(arr))));
    },
    nested: (() => {
      const $1 = Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjCons(Codec$dJson$dUnidirectional$dValue.fromRecordObjNil)({
        reflectSymbol: () => "other"
      })()())({reflectSymbol: () => "foo"})()();
      const $2 = {
        other: (() => {
          const $2 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
          return x => Data$dMaybe.$Maybe("Just", $2(Data$dArgonaut$dCore.fromBoolean(x)));
        })(),
        foo: Data$dMaybe.functorMaybe.map((() => {
          const $2 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
          return x => $2(Data$dArgonaut$dCore.fromBoolean(x));
        })())
      };
      const $3 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      return x => Data$dMaybe.$Maybe("Just", $3(Data$dArgonaut$dCore.fromObject($1.fromRecordObj(Type$dProxy.Proxy)($2)(x))));
    })()
  };
  return values => Data$dArgonaut$dCore.fromObject($0.fromRecordObj(Type$dProxy.Proxy)($1)(values));
})();
const test = j => Data$dArgonaut$dCore.fromObject((() => {
  const $0 = Data$dArgonaut$dCore.fromString(j.reqRen);
  const $1 = Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(j.req));
  const $2 = Data$dArgonaut$dCore.fromObject((() => {
    const $2 = Data$dArgonaut$dCore.fromBoolean(j.nested.other);
    const obj = Foreign$dObject.mutate($3 => () => {
      $3.other = $2;
      return $3;
    })(Foreign$dObject.empty);
    const $3 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
    if (j.nested.foo.tag === "Just") {
      const $4 = $3(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._2;
      const $5 = (() => {
        if ($3(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1.tag === "Nothing") { return "foo"; }
        if ($3(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1.tag === "Just") { return $3(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1._1; }
        $runtime.fail();
      })();
      return Foreign$dObject.mutate($6 => () => {
        $6[$5] = $4;
        return $6;
      })(obj);
    }
    return obj;
  })());
  return Foreign$dObject.mutate($3 => () => {
    $3.nested = $2;
    return $3;
  })((() => {
    const obj = Foreign$dObject.mutate($3 => () => {
      $3.req = $1;
      return $3;
    })(Foreign$dObject.mutate($3 => () => {
      $3.otherName = $0;
      return $3;
    })(Foreign$dObject.empty));
    const $3 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
    const obj$1 = (() => {
      if (j.optRen.tag === "Just") {
        const $4 = $3(Data$dArgonaut$dCore.fromString(j.optRen._1))._2;
        const $5 = (() => {
          if ($3(Data$dArgonaut$dCore.fromString(j.optRen._1))._1.tag === "Nothing") { return "optRen"; }
          if ($3(Data$dArgonaut$dCore.fromString(j.optRen._1))._1.tag === "Just") { return $3(Data$dArgonaut$dCore.fromString(j.optRen._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($6 => () => {
          $6[$5] = $4;
          return $6;
        })(obj);
      }
      return obj;
    })();
    const obj$2 = (() => {
      if (j.optArr.length === 0) { return obj$1; }
      const $4 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(j.optArr));
      return Foreign$dObject.mutate($5 => () => {
        $5.optArr = $4;
        return $5;
      })(obj$1);
    })();
    const $4 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
    if (j.opt.tag === "Just") {
      const $5 = $4(Data$dArgonaut$dCore.fromString(j.opt._1))._2;
      const $6 = (() => {
        if ($4(Data$dArgonaut$dCore.fromString(j.opt._1))._1.tag === "Nothing") { return "opt"; }
        if ($4(Data$dArgonaut$dCore.fromString(j.opt._1))._1.tag === "Just") { return $4(Data$dArgonaut$dCore.fromString(j.opt._1))._1._1; }
        $runtime.fail();
      })();
      return Foreign$dObject.mutate($7 => () => {
        $7[$6] = $5;
        return $7;
      })(obj$2);
    }
    return obj$2;
  })());
})());
export {encoder, test};
