// @inline export Snapshot.ToRecordInlines.decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dDecoders$dSpeedyDecoder from "../Codec.Json.Decoders.SpeedyDecoder/index.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
const reqRenIsSymbol = {reflectSymbol: () => "reqRen"};
const Reflectable$Dict = {reflectType: () => "reqRen"};
const reqIsSymbol = {reflectSymbol: () => "req"};
const Reflectable$Dict1 = {reflectType: () => "req"};
const optRenIsSymbol = {reflectSymbol: () => "optRen"};
const Reflectable$Dict2 = {reflectType: () => "optRen"};
const optArrIsSymbol = {reflectSymbol: () => "optArr"};
const Reflectable$Dict3 = {reflectType: () => "optArr"};
const optIsSymbol = {reflectSymbol: () => "opt"};
const Reflectable$Dict4 = {reflectType: () => "opt"};
const nestedIsSymbol = {reflectSymbol: () => "nested"};
const Reflectable$Dict5 = {reflectType: () => "nested"};
const otherIsSymbol = {reflectSymbol: () => "other"};
const Reflectable$Dict6 = {reflectType: () => "other"};
const fooIsSymbol = {reflectSymbol: () => "foo"};
const Reflectable$Dict7 = {reflectType: () => "foo"};
const decoder = dictIsJsonDecoder => {
  const toRecordObjCons = Codec$dJson$dUnidirectional$dValue.toRecordObjCons({toRecordObj: v => v1 => v2 => dictIsJsonDecoder.Monad0().Applicative0().pure({})});
  const toString = Codec$dJson$dUnidirectional$dValue.toString(dictIsJsonDecoder);
  const toBoolean = Codec$dJson$dUnidirectional$dValue.toBoolean(dictIsJsonDecoder);
  return Codec$dJson$dUnidirectional$dValue.toRecord()(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(toRecordObjCons(reqRenIsSymbol)(Reflectable$Dict)(dictIsJsonDecoder)()()())(reqIsSymbol)(Reflectable$Dict1)(dictIsJsonDecoder)()()())(optRenIsSymbol)(Reflectable$Dict2)(dictIsJsonDecoder)()()())(optArrIsSymbol)(Reflectable$Dict3)(dictIsJsonDecoder)()()())(optIsSymbol)(Reflectable$Dict4)(dictIsJsonDecoder)()()())(nestedIsSymbol)(Reflectable$Dict5)(dictIsJsonDecoder)()()())(dictIsJsonDecoder)({
    req: Codec$dJson$dUnidirectional$dValue.toRequired(dictIsJsonDecoder)(Codec$dJson$dUnidirectional$dValue.toInt(dictIsJsonDecoder)),
    reqRen: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.$Maybe("Just", "otherName"),
          k => j => {
            if (j.tag === "Nothing") { return dictIsJsonDecoder.onMissingField(k); }
            if (j.tag === "Just") { return dictIsJsonDecoder.atKey(k)(toString(j._1)); }
            $runtime.fail();
          }
        )
      ]
    ),
    opt: Codec$dJson$dUnidirectional$dValue.toOption(dictIsJsonDecoder)(toString),
    optRen: Codec$dJson$dUnidirectional$dValue.toOptionRename(dictIsJsonDecoder)("otherName2")(toString),
    optArr: Codec$dJson$dUnidirectional$dValue.toOptionArray(dictIsJsonDecoder)(toString),
    nested: Codec$dJson$dUnidirectional$dValue.toRequired(dictIsJsonDecoder)(Codec$dJson$dUnidirectional$dValue.toRecord()(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(toRecordObjCons(otherIsSymbol)(Reflectable$Dict6)(dictIsJsonDecoder)()()())(fooIsSymbol)(Reflectable$Dict7)(dictIsJsonDecoder)()()())(dictIsJsonDecoder)({
      other: Codec$dJson$dUnidirectional$dValue.toRequired(dictIsJsonDecoder)(toBoolean),
      foo: Codec$dJson$dUnidirectional$dValue.toOption(dictIsJsonDecoder)(toBoolean)
    }))
  });
};
const decoder1 = /* #__PURE__ */ decoder(Codec$dJson$dDecoders$dSpeedyDecoder.isJsonDecoderSpeedyDecoder);
const test = j => decoder1(j);
export {
  Reflectable$Dict,
  Reflectable$Dict1,
  Reflectable$Dict2,
  Reflectable$Dict3,
  Reflectable$Dict4,
  Reflectable$Dict5,
  Reflectable$Dict6,
  Reflectable$Dict7,
  decoder,
  decoder1,
  fooIsSymbol,
  nestedIsSymbol,
  optArrIsSymbol,
  optIsSymbol,
  optRenIsSymbol,
  otherIsSymbol,
  reqIsSymbol,
  reqRenIsSymbol,
  test
};
