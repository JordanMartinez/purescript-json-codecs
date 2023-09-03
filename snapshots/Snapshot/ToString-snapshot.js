// @inline export Snapshot.ToString.decoder arity=1
import * as Codec$dJson$dDecoders$dSpeedyDecoder from "../Codec.Json.Decoders.SpeedyDecoder/index.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
const decoder = dictIsJsonDecoder => Codec$dJson$dUnidirectional$dValue.toString(dictIsJsonDecoder);
const decoder1 = /* #__PURE__ */ Codec$dJson$dUnidirectional$dValue.toString(Codec$dJson$dDecoders$dSpeedyDecoder.isJsonDecoderSpeedyDecoder);
const test = j => decoder1(j);
export {decoder, decoder1, test};
