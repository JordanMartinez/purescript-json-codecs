// @inline export decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const Foo = x => x;
const newtypeFoo_ = {Coercible0: () => {}};
const decoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "static"
  })()()())({reflectSymbol: () => "reqRen"})()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optDefRen"})()()())({
    reflectSymbol: () => "optDef"
  })()()())({reflectSymbol: () => "optAssocArr"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()())({reflectSymbol: () => "nested"})()()().toRecordObj(Type$dProxy.Proxy)({
    static: (v, v1) => Data$dEither.$Either("Right", 1),
    req: Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toInt),
    reqRen: Codec$dJson$dUnidirectional$dValue.toRequiredRename("otherName")(Codec$dJson$dUnidirectional$dValue.toString),
    opt: Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toString),
    optRen: Codec$dJson$dUnidirectional$dValue.toOptionRename("otherName2")(Codec$dJson$dUnidirectional$dValue.toString),
    optDef: Codec$dJson$dUnidirectional$dValue.toOptionDefault(1)(Codec$dJson$dUnidirectional$dValue.toInt),
    optDefRen: Codec$dJson$dUnidirectional$dValue.toOptionDefaultRename("otherName3")(2)(Codec$dJson$dUnidirectional$dValue.toInt),
    optArr: Codec$dJson$dUnidirectional$dValue.toOptionArray(Codec$dJson$dUnidirectional$dValue.toString),
    optAssocArr: Codec$dJson$dUnidirectional$dValue.toOptionAssocArray(Data$dEither.Right)(Codec$dJson$dUnidirectional$dValue.toString),
    nested: Codec$dJson$dUnidirectional$dValue.toRequired((() => {
      const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
        reflectSymbol: () => "other"
      })()()())({reflectSymbol: () => "foo"})()()().toRecordObj(Type$dProxy.Proxy)({
        other: Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toBoolean),
        foo: Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toBoolean)
      });
      return a => {
        const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
        if ($1.tag === "Right") { return $0($1._1); }
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
        $runtime.fail();
      };
    })())
  });
  return a => {
    const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
    if ($1.tag === "Right") { return $0($1._1); }
    if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
    $runtime.fail();
  };
})();
const test = j => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "static"
  })()()())({reflectSymbol: () => "reqRen"})()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optDefRen"})()()())({
    reflectSymbol: () => "optDef"
  })()()())({reflectSymbol: () => "optAssocArr"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()())({reflectSymbol: () => "nested"})()()().toRecordObj(Type$dProxy.Proxy)({
    static: (v, v1) => Data$dEither.$Either("Right", 1),
    req: Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toInt),
    reqRen: Codec$dJson$dUnidirectional$dValue.toRequiredRename("otherName")(Codec$dJson$dUnidirectional$dValue.toString),
    opt: Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toString),
    optRen: Codec$dJson$dUnidirectional$dValue.toOptionRename("otherName2")(Codec$dJson$dUnidirectional$dValue.toString),
    optDef: Codec$dJson$dUnidirectional$dValue.toOptionDefault(1)(Codec$dJson$dUnidirectional$dValue.toInt),
    optDefRen: Codec$dJson$dUnidirectional$dValue.toOptionDefaultRename("otherName3")(2)(Codec$dJson$dUnidirectional$dValue.toInt),
    optArr: Codec$dJson$dUnidirectional$dValue.toOptionArray(Codec$dJson$dUnidirectional$dValue.toString),
    optAssocArr: Codec$dJson$dUnidirectional$dValue.toOptionAssocArray(Data$dEither.Right)(Codec$dJson$dUnidirectional$dValue.toString),
    nested: Codec$dJson$dUnidirectional$dValue.toRequired((() => {
      const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
        reflectSymbol: () => "other"
      })()()())({reflectSymbol: () => "foo"})()()().toRecordObj(Type$dProxy.Proxy)({
        other: Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toBoolean),
        foo: Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toBoolean)
      });
      return a => {
        const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
        if ($1.tag === "Right") { return $0($1._1); }
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
        $runtime.fail();
      };
    })())
  });
  const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(j);
  if ($1.tag === "Right") { return $0($1._1); }
  if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
  $runtime.fail();
};
export {Foo, decoder, newtypeFoo_, test};
