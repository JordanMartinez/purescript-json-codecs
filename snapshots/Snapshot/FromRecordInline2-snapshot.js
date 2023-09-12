// @inline export encoder arity=1
import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const encoder = /* #__PURE__ */ (() => {
  const $0 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
  return values => Data$dArgonaut$dCore.fromObject((() => {
    const $1 = $0(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(values.req)));
    const $2 = $1._2;
    const $3 = (() => {
      if ($1._1.tag === "Nothing") { return "req"; }
      if ($1._1.tag === "Just") { return $1._1._1; }
      $runtime.fail();
    })();
    return Foreign$dObject.mutate($4 => () => {
      $4[$3] = $2;
      return $4;
    })(Foreign$dObject.empty);
  })());
})();
const test = j => Data$dArgonaut$dCore.fromObject((() => {
  const $0 = Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(j.req));
  return Foreign$dObject.mutate($1 => () => {
    $1.req = $0;
    return $1;
  })(Foreign$dObject.empty);
})());
export {encoder, test};
