// @inline export decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
const decoder = a => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", {}); }
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  $runtime.fail();
};
const test = j => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toJObject(j);
  if ($0.tag === "Right") { return Data$dEither.$Either("Right", {}); }
  if ($0.tag === "Left") { return Data$dEither.$Either("Left", $0._1); }
  $runtime.fail();
};
export {decoder, test};
