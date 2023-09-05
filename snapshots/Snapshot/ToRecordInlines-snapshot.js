// @inline export Snapshot.ToRecordInlines.decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
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
              return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field, " + Data$dShow.showStringImpl(k)));
            }
            if (j.tag === "Just") {
              const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
              const $1 = Codec$dJson$dUnidirectional$dValue.toInt(j._1);
              if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
              if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
            }
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
              return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field, " + Data$dShow.showStringImpl(k)));
            }
            if (j.tag === "Just") {
              const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
              const $1 = Codec$dJson$dUnidirectional$dValue.toString(j._1);
              if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
              if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
            }
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
              const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
              const $1 = Codec$dJson$dUnidirectional$dValue.toString(j._1);
              if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
              if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
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
              const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
              const $1 = Codec$dJson$dUnidirectional$dValue.toString(j._1);
              if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
              if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
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
                  return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field, " + Data$dShow.showStringImpl(k)));
                }
                if (j.tag === "Just") {
                  const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
                  const $1 = Codec$dJson$dUnidirectional$dValue.toBoolean(j._1);
                  if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
                  if ($1.tag === "Right") { return Data$dEither.$Either("Right", $1._1); }
                }
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
                  const $0 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
                  const $1 = Codec$dJson$dUnidirectional$dValue.toBoolean(j._1);
                  if ($1.tag === "Left") { return Data$dEither.$Either("Left", $0($1._1)); }
                  if ($1.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $1._1)); }
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
                return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field, " + Data$dShow.showStringImpl(k)));
              }
              if (j.tag === "Just") {
                const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(k);
                const $2 = Codec$dJson$dUnidirectional$dValue.toJObject(j._1);
                const $3 = (() => {
                  if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
                  if ($2.tag === "Right") { return $0($2._1); }
                  $runtime.fail();
                })();
                if ($3.tag === "Left") { return Data$dEither.$Either("Left", $1($3._1)); }
                if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
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
