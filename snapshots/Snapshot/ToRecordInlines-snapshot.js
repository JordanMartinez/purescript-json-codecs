// @inline export Snapshot.ToRecordInlines.decoder arity=1
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
const decoder = /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecord()(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
  reflectSymbol: () => "reqRen"
})({reflectType: () => "reqRen"})()()())({reflectSymbol: () => "req"})({reflectType: () => "req"})()()())({reflectSymbol: () => "optRen"})({reflectType: () => "optRen"})()()())({
  reflectSymbol: () => "optArr"
})({reflectType: () => "optArr"})()()())({reflectSymbol: () => "opt"})({reflectType: () => "opt"})()()())({reflectSymbol: () => "nested"})({reflectType: () => "nested"})()()())({
  req: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toInt),
  reqRen: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRequiredRename("otherName")(Codec$dJson$dUnidirectional$dValue.toString),
  opt: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toString),
  optRen: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toOptionRename("otherName2")(Codec$dJson$dUnidirectional$dValue.toString),
  optArr: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toOptionArray(Codec$dJson$dUnidirectional$dValue.toString),
  nested: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRequired(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecord()(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(/* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "other"
  })({reflectType: () => "other"})()()())({reflectSymbol: () => "foo"})({reflectType: () => "foo"})()()())({
    other: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toBoolean),
    foo: /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toOption(Codec$dJson$dUnidirectional$dValue.toBoolean)
  }))
});
const test = j => decoder(j);
export {decoder, test};
