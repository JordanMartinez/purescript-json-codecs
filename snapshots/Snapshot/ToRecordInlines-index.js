// Generated by purs version 0.15.10
import * as Codec_Json_Unidirectional_Value from "../Codec.Json.Unidirectional.Value/index.js";
var toRecord = /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecord();
var toRecordObjCons = /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(Codec_Json_Unidirectional_Value.toRecordObjNil);
var decoder = /* #__PURE__ */ toRecord(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ toRecordObjCons({
    reflectSymbol: function () {
        return "reqRen";
    }
})()()())({
    reflectSymbol: function () {
        return "req";
    }
})()()())({
    reflectSymbol: function () {
        return "optRen";
    }
})()()())({
    reflectSymbol: function () {
        return "optArr";
    }
})()()())({
    reflectSymbol: function () {
        return "opt";
    }
})()()())({
    reflectSymbol: function () {
        return "nested";
    }
})()()())({
    req: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRequired(Codec_Json_Unidirectional_Value.toInt),
    reqRen: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRequiredRename("otherName")(Codec_Json_Unidirectional_Value.toString),
    opt: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toOption(Codec_Json_Unidirectional_Value.toString),
    optRen: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toOptionRename("otherName2")(Codec_Json_Unidirectional_Value.toString),
    optArr: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toOptionArray(Codec_Json_Unidirectional_Value.toString),
    nested: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRequired(/* #__PURE__ */ toRecord(/* #__PURE__ */ Codec_Json_Unidirectional_Value.toRecordObjCons(/* #__PURE__ */ toRecordObjCons({
        reflectSymbol: function () {
            return "other";
        }
    })()()())({
        reflectSymbol: function () {
            return "foo";
        }
    })()()())({
        other: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toRequired(Codec_Json_Unidirectional_Value.toBoolean),
        foo: /* #__PURE__ */ Codec_Json_Unidirectional_Value.toOption(Codec_Json_Unidirectional_Value.toBoolean)
    }))
});
var test = function (j) {
    return decoder(j);
};
export {
    decoder,
    test
};
