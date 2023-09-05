// @inline export Snapshot.ToRecordInlines.decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const decoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "reqRen"
  })()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()())({
    reflectSymbol: () => "nested"
  })()()().toRecordObj(Type$dProxy.Proxy)({
    req: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") {
        return Data$dEither.$Either(
          "Left",
          Codec$dJson$dUnidirectional$dValue.$DecodeError("AtKey", recLabel, Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field"))
        );
      }
      if (v.tag === "Just") {
        const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $1 = Codec$dJson$dUnidirectional$dValue.toInt(v._1);
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
        if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
      }
      $runtime.fail();
    },
    reqRen: (lookupFn, v) => {
      const v1 = lookupFn("otherName");
      if (v1.tag === "Nothing") {
        return Data$dEither.$Either(
          "Left",
          Codec$dJson$dUnidirectional$dValue.$DecodeError("AtKey", "otherName", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field"))
        );
      }
      if (v1.tag === "Just") {
        const $0 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName");
        const $1 = Codec$dJson$dUnidirectional$dValue.toString(v1._1);
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
        if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
      }
      $runtime.fail();
    },
    opt: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v.tag === "Just") {
        const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $1 = Codec$dJson$dUnidirectional$dValue.toString(v._1);
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
        if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
      }
      $runtime.fail();
    },
    optRen: (lookupFn, v) => {
      const v1 = lookupFn("otherName2");
      if (v1.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v1.tag === "Just") {
        const $0 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName2");
        const $1 = Codec$dJson$dUnidirectional$dValue.toString(v1._1);
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
        if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
      }
      $runtime.fail();
    },
    optArr: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", []); }
      if (v.tag === "Just") {
        const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $1 = Codec$dJson$dUnidirectional$dValue.toArray(Codec$dJson$dUnidirectional$dValue.toString)(v._1);
        if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
        if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
      }
      $runtime.fail();
    },
    nested: (() => {
      const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
        reflectSymbol: () => "other"
      })()()())({reflectSymbol: () => "foo"})()()().toRecordObj(Type$dProxy.Proxy)({
        other: (lookupFn, recLabel) => {
          const v = lookupFn(recLabel);
          if (v.tag === "Nothing") {
            return Data$dEither.$Either(
              "Left",
              Codec$dJson$dUnidirectional$dValue.$DecodeError("AtKey", recLabel, Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field"))
            );
          }
          if (v.tag === "Just") {
            const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
            const $1 = Codec$dJson$dUnidirectional$dValue.toBoolean(v._1);
            if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
            if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
          }
          $runtime.fail();
        },
        foo: (lookupFn, recLabel) => {
          const v = lookupFn(recLabel);
          if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
          if (v.tag === "Just") {
            const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
            const $1 = Codec$dJson$dUnidirectional$dValue.toBoolean(v._1);
            if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
            if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
          }
          $runtime.fail();
        }
      });
      return (lookupFn, recLabel) => {
        const v = lookupFn(recLabel);
        if (v.tag === "Nothing") {
          return Data$dEither.$Either(
            "Left",
            Codec$dJson$dUnidirectional$dValue.$DecodeError("AtKey", recLabel, Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field"))
          );
        }
        if (v.tag === "Just") {
          const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
          const $2 = Codec$dJson$dUnidirectional$dValue.toJObject(v._1);
          const $3 = (() => {
            if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
            if ($2.tag === "Right") { return $0($2._1); }
            $runtime.fail();
          })();
          if ($3.tag === "Left") { return Data$dEither.$Either("Left", $1($3._1)); }
          if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
        }
        $runtime.fail();
      };
    })()
  });
  return a => {
    const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
    if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
    if ($1.tag === "Right") { return $0($1._1); }
    $runtime.fail();
  };
})();
const test = j => decoder(j);
export {decoder, test};
