// @inline export encoder arity=1
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const encoder = values => Data$dArgonaut$dCore.fromObject(Foreign$dObject.empty);
const test = j => Data$dArgonaut$dCore.fromObject(Foreign$dObject.empty);
export {encoder, test};
