// @inline export decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const encoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({reflectSymbol: () => "req"})()()().toRecordObj(Type$dProxy.Proxy)({
    req: Codec$dJson$dUnidirectional$dValue.toRequired(Codec$dJson$dUnidirectional$dValue.toInt)
  });
  return a => {
    const $1 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
    if ($1.tag === "Right") { return $0($1._1); }
    if ($1.tag === "Left") { return Data$dEither.$Either("Left", $1._1); }
    $runtime.fail();
  };
})();
const test = j => encoder(j);
export {encoder, test};
