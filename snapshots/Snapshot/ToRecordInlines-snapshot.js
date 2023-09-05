// @inline export Snapshot.ToRecordInlines.decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const decoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "reqRen"
  })()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()())({
    reflectSymbol: () => "nested"
  })()()().toRecordObj(Type$dProxy.Proxy)({
    req: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.Nothing,
          k => j => {
            if (j.tag === "Nothing") {
              return Data$dEither.$Either("Left", Data$dList$dTypes.$List("Cons", "Missing field, " + Data$dShow.showStringImpl(k), Data$dList$dTypes.Nil));
            }
            if (j.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.withKey(k)(Codec$dJson$dUnidirectional$dValue.toInt(j._1)); }
            $runtime.fail();
          }
        )
      ]
    ),
    reqRen: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.$Maybe("Just", "otherName"),
          k => j => {
            if (j.tag === "Nothing") {
              return Data$dEither.$Either("Left", Data$dList$dTypes.$List("Cons", "Missing field, " + Data$dShow.showStringImpl(k), Data$dList$dTypes.Nil));
            }
            if (j.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.withKey(k)(Codec$dJson$dUnidirectional$dValue.toString(j._1)); }
            $runtime.fail();
          }
        )
      ]
    ),
    opt: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.Nothing,
          k => j => {
            if (j.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
            if (j.tag === "Just") {
              return Codec$dJson$dUnidirectional$dValue.withKey(k)((() => {
                const $0 = Codec$dJson$dUnidirectional$dValue.toString(j._1);
                if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
                if ($0.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $0._1)); }
                $runtime.fail();
              })());
            }
            $runtime.fail();
          }
        )
      ]
    ),
    optRen: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.$Maybe("Just", "otherName2"),
          k => j => {
            if (j.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
            if (j.tag === "Just") {
              return Codec$dJson$dUnidirectional$dValue.withKey(k)((() => {
                const $0 = Codec$dJson$dUnidirectional$dValue.toString(j._1);
                if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
                if ($0.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $0._1)); }
                $runtime.fail();
              })());
            }
            $runtime.fail();
          }
        )
      ]
    ),
    optArr: Data$dEither.$Either(
      "Right",
      [
        Data$dTuple.$Tuple(
          Data$dMaybe.Nothing,
          v => j => {
            if (j.tag === "Nothing") { return Data$dEither.$Either("Right", []); }
            if (j.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toArray(Codec$dJson$dUnidirectional$dValue.toString)(j._1); }
            $runtime.fail();
          }
        )
      ]
    ),
    nested: (() => {
      const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
        reflectSymbol: () => "other"
      })()()())({reflectSymbol: () => "foo"})()()().toRecordObj(Type$dProxy.Proxy)({
        other: Data$dEither.$Either(
          "Right",
          [
            Data$dTuple.$Tuple(
              Data$dMaybe.Nothing,
              k => j => {
                if (j.tag === "Nothing") {
                  return Data$dEither.$Either("Left", Data$dList$dTypes.$List("Cons", "Missing field, " + Data$dShow.showStringImpl(k), Data$dList$dTypes.Nil));
                }
                if (j.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.withKey(k)(Codec$dJson$dUnidirectional$dValue.toBoolean(j._1)); }
                $runtime.fail();
              }
            )
          ]
        ),
        foo: Data$dEither.$Either(
          "Right",
          [
            Data$dTuple.$Tuple(
              Data$dMaybe.Nothing,
              k => j => {
                if (j.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
                if (j.tag === "Just") {
                  return Codec$dJson$dUnidirectional$dValue.withKey(k)((() => {
                    const $0 = Codec$dJson$dUnidirectional$dValue.toBoolean(j._1);
                    if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
                    if ($0.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $0._1)); }
                    $runtime.fail();
                  })());
                }
                $runtime.fail();
              }
            )
          ]
        )
      });
      return Data$dEither.$Either(
        "Right",
        [
          Data$dTuple.$Tuple(
            Data$dMaybe.Nothing,
            k => j => {
              if (j.tag === "Nothing") {
                return Data$dEither.$Either("Left", Data$dList$dTypes.$List("Cons", "Missing field, " + Data$dShow.showStringImpl(k), Data$dList$dTypes.Nil));
              }
              if (j.tag === "Just") {
                return Codec$dJson$dUnidirectional$dValue.withKey(k)((() => {
                  const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(j._1);
                  if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
                  if ($1.tag === "Right") { return $0($1._1); }
                  $runtime.fail();
                })());
              }
              $runtime.fail();
            }
          )
        ]
      );
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
