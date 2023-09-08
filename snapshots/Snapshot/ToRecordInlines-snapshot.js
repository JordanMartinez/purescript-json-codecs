// @inline export decoder arity=1
import * as $runtime from "../runtime.js";
import * as Codec$dJson$dUnidirectional$dValue from "../Codec.Json.Unidirectional.Value/index.js";
import * as Data$dEither from "../Data.Either/index.js";
import * as Data$dList$dTypes from "../Data.List.Types/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
import * as Type$dProxy from "../Type.Proxy/index.js";
const decoder = /* #__PURE__ */ (() => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "reqRen"
  })()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()();
  const $1 = {
    req: (lookupFn, recLabel) => {
      const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
      const v = lookupFn(recLabel);
      const $2 = (() => {
        if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
        if (v.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toInt(v._1); }
        $runtime.fail();
      })();
      if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
      if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      $runtime.fail();
    },
    reqRen: (lookupFn, v) => {
      const $1 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName");
      const v1 = lookupFn("otherName");
      const $2 = (() => {
        if (v1.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
        if (v1.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toString(v1._1); }
        $runtime.fail();
      })();
      if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
      if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      $runtime.fail();
    },
    opt: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $2 = Codec$dJson$dUnidirectional$dValue.toString(v._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
      }
      $runtime.fail();
    },
    optRen: (lookupFn, v) => {
      const v1 = lookupFn("otherName2");
      if (v1.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v1.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName2");
        const $2 = Codec$dJson$dUnidirectional$dValue.toString(v1._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
      }
      $runtime.fail();
    },
    optArr: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", []); }
      if (v.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $2 = Codec$dJson$dUnidirectional$dValue.toArray(Codec$dJson$dUnidirectional$dValue.toString)(v._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      }
      $runtime.fail();
    },
    nested: (() => {
      const $1 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({reflectSymbol: () => "other"})()()();
      const $2 = {
        other: (lookupFn, recLabel) => {
          const $2 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
          const v = lookupFn(recLabel);
          const $3 = (() => {
            if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
            if (v.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toBoolean(v._1); }
            $runtime.fail();
          })();
          if ($3.tag === "Left") { return Data$dEither.$Either("Left", $2($3._1)); }
          if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
          $runtime.fail();
        },
        foo: (lookupFn, recLabel) => {
          const v = lookupFn(recLabel);
          if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
          if (v.tag === "Just") {
            const $2 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
            const $3 = Codec$dJson$dUnidirectional$dValue.toBoolean(v._1);
            if ($3.tag === "Left") { return Data$dEither.$Either("Left", $2($3._1)); }
            if ($3.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $3._1)); }
          }
          $runtime.fail();
        }
      };
      return (lookupFn, recLabel) => {
        const $3 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const v = lookupFn(recLabel);
        const $4 = (() => {
          if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
          if (v.tag === "Just") {
            const $4 = Codec$dJson$dUnidirectional$dValue.toJObject(v._1);
            if ($4.tag === "Left") { return Data$dEither.$Either("Left", $4._1); }
            if ($4.tag === "Right") {
              const $5 = $1.toRecordObj(Type$dProxy.Proxy)($2)($4._1);
              if ($5.tag === "Left") {
                const v2 = $2.foo(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $4._1), "foo");
                if (v2.tag === "Left") {
                  return Data$dEither.$Either(
                    "Left",
                    $5._1.tag === "AccumulateError"
                      ? Codec$dJson$dUnidirectional$dValue.$DecodeError("AccumulateError", Data$dList$dTypes.$List("Cons", v2._1, $5._1._1))
                      : Codec$dJson$dUnidirectional$dValue.$DecodeError(
                          "AccumulateError",
                          Data$dList$dTypes.$List("Cons", v2._1, Data$dList$dTypes.$List("Cons", $5._1, Data$dList$dTypes.Nil))
                        )
                  );
                }
                return Data$dEither.$Either("Left", $5._1);
              }
              if ($5.tag === "Right") {
                return (() => {
                  if ($5.tag === "Left") {
                    const $6 = $5._1;
                    return v$1 => Data$dEither.$Either("Left", $6);
                  }
                  if ($5.tag === "Right") {
                    const $6 = $5._1;
                    return f => f($6);
                  }
                  $runtime.fail();
                })()(rec => {
                  const $6 = $2.foo(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $4._1), "foo");
                  return (() => {
                    if ($6.tag === "Left") {
                      const $7 = $6._1;
                      return v$1 => Data$dEither.$Either("Left", $7);
                    }
                    if ($6.tag === "Right") {
                      const $7 = $6._1;
                      return f => f($7);
                    }
                    $runtime.fail();
                  })()(a => Data$dEither.$Either("Right", {...rec, foo: a}));
                });
              }
            }
          }
          $runtime.fail();
        })();
        if ($4.tag === "Left") { return Data$dEither.$Either("Left", $3($4._1)); }
        if ($4.tag === "Right") { return Data$dEither.$Either("Right", $4._1); }
        $runtime.fail();
      };
    })()
  };
  return a => {
    const $2 = Codec$dJson$dUnidirectional$dValue.toJObject(a);
    if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
    if ($2.tag === "Right") {
      const $3 = $0.toRecordObj(Type$dProxy.Proxy)($1)($2._1);
      if ($3.tag === "Left") {
        const v2 = $1.nested(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $2._1), "nested");
        if (v2.tag === "Left") {
          return Data$dEither.$Either(
            "Left",
            $3._1.tag === "AccumulateError"
              ? Codec$dJson$dUnidirectional$dValue.$DecodeError("AccumulateError", Data$dList$dTypes.$List("Cons", v2._1, $3._1._1))
              : Codec$dJson$dUnidirectional$dValue.$DecodeError(
                  "AccumulateError",
                  Data$dList$dTypes.$List("Cons", v2._1, Data$dList$dTypes.$List("Cons", $3._1, Data$dList$dTypes.Nil))
                )
          );
        }
        return Data$dEither.$Either("Left", $3._1);
      }
      if ($3.tag === "Right") {
        return (() => {
          if ($3.tag === "Left") {
            const $4 = $3._1;
            return v => Data$dEither.$Either("Left", $4);
          }
          if ($3.tag === "Right") {
            const $4 = $3._1;
            return f => f($4);
          }
          $runtime.fail();
        })()(rec => {
          const $4 = $1.nested(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $2._1), "nested");
          return (() => {
            if ($4.tag === "Left") {
              const $5 = $4._1;
              return v => Data$dEither.$Either("Left", $5);
            }
            if ($4.tag === "Right") {
              const $5 = $4._1;
              return f => f($5);
            }
            $runtime.fail();
          })()(a$1 => Data$dEither.$Either("Right", {...rec, nested: a$1}));
        });
      }
    }
    $runtime.fail();
  };
})();
const test = j => {
  const $0 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({
    reflectSymbol: () => "reqRen"
  })()()())({reflectSymbol: () => "req"})()()())({reflectSymbol: () => "optRen"})()()())({reflectSymbol: () => "optArr"})()()())({reflectSymbol: () => "opt"})()()();
  const $1 = {
    req: (lookupFn, recLabel) => {
      const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
      const v = lookupFn(recLabel);
      const $2 = (() => {
        if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
        if (v.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toInt(v._1); }
        $runtime.fail();
      })();
      if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
      if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      $runtime.fail();
    },
    reqRen: (lookupFn, v) => {
      const $1 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName");
      const v1 = lookupFn("otherName");
      const $2 = (() => {
        if (v1.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
        if (v1.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toString(v1._1); }
        $runtime.fail();
      })();
      if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
      if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      $runtime.fail();
    },
    opt: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $2 = Codec$dJson$dUnidirectional$dValue.toString(v._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
      }
      $runtime.fail();
    },
    optRen: (lookupFn, v) => {
      const v1 = lookupFn("otherName2");
      if (v1.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
      if (v1.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey("otherName2");
        const $2 = Codec$dJson$dUnidirectional$dValue.toString(v1._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $2._1)); }
      }
      $runtime.fail();
    },
    optArr: (lookupFn, recLabel) => {
      const v = lookupFn(recLabel);
      if (v.tag === "Nothing") { return Data$dEither.$Either("Right", []); }
      if (v.tag === "Just") {
        const $1 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const $2 = Codec$dJson$dUnidirectional$dValue.toArray(Codec$dJson$dUnidirectional$dValue.toString)(v._1);
        if ($2.tag === "Left") { return Data$dEither.$Either("Left", $1($2._1)); }
        if ($2.tag === "Right") { return Data$dEither.$Either("Right", $2._1); }
      }
      $runtime.fail();
    },
    nested: (() => {
      const $1 = Codec$dJson$dUnidirectional$dValue.toRecordObjCons(Codec$dJson$dUnidirectional$dValue.toRecordObjNil)({reflectSymbol: () => "other"})()()();
      const $2 = {
        other: (lookupFn, recLabel) => {
          const $2 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
          const v = lookupFn(recLabel);
          const $3 = (() => {
            if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
            if (v.tag === "Just") { return Codec$dJson$dUnidirectional$dValue.toBoolean(v._1); }
            $runtime.fail();
          })();
          if ($3.tag === "Left") { return Data$dEither.$Either("Left", $2($3._1)); }
          if ($3.tag === "Right") { return Data$dEither.$Either("Right", $3._1); }
          $runtime.fail();
        },
        foo: (lookupFn, recLabel) => {
          const v = lookupFn(recLabel);
          if (v.tag === "Nothing") { return Data$dEither.$Either("Right", Data$dMaybe.Nothing); }
          if (v.tag === "Just") {
            const $2 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
            const $3 = Codec$dJson$dUnidirectional$dValue.toBoolean(v._1);
            if ($3.tag === "Left") { return Data$dEither.$Either("Left", $2($3._1)); }
            if ($3.tag === "Right") { return Data$dEither.$Either("Right", Data$dMaybe.$Maybe("Just", $3._1)); }
          }
          $runtime.fail();
        }
      };
      return (lookupFn, recLabel) => {
        const $3 = Codec$dJson$dUnidirectional$dValue.AtKey(recLabel);
        const v = lookupFn(recLabel);
        const $4 = (() => {
          if (v.tag === "Nothing") { return Data$dEither.$Either("Left", Codec$dJson$dUnidirectional$dValue.$DecodeError("DecodeError", "Missing field")); }
          if (v.tag === "Just") {
            const $4 = Codec$dJson$dUnidirectional$dValue.toJObject(v._1);
            if ($4.tag === "Left") { return Data$dEither.$Either("Left", $4._1); }
            if ($4.tag === "Right") {
              const $5 = $1.toRecordObj(Type$dProxy.Proxy)($2)($4._1);
              if ($5.tag === "Left") {
                const v2 = $2.foo(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $4._1), "foo");
                if (v2.tag === "Left") {
                  return Data$dEither.$Either(
                    "Left",
                    $5._1.tag === "AccumulateError"
                      ? Codec$dJson$dUnidirectional$dValue.$DecodeError("AccumulateError", Data$dList$dTypes.$List("Cons", v2._1, $5._1._1))
                      : Codec$dJson$dUnidirectional$dValue.$DecodeError(
                          "AccumulateError",
                          Data$dList$dTypes.$List("Cons", v2._1, Data$dList$dTypes.$List("Cons", $5._1, Data$dList$dTypes.Nil))
                        )
                  );
                }
                return Data$dEither.$Either("Left", $5._1);
              }
              if ($5.tag === "Right") {
                return (() => {
                  if ($5.tag === "Left") {
                    const $6 = $5._1;
                    return v$1 => Data$dEither.$Either("Left", $6);
                  }
                  if ($5.tag === "Right") {
                    const $6 = $5._1;
                    return f => f($6);
                  }
                  $runtime.fail();
                })()(rec => {
                  const $6 = $2.foo(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $4._1), "foo");
                  return (() => {
                    if ($6.tag === "Left") {
                      const $7 = $6._1;
                      return v$1 => Data$dEither.$Either("Left", $7);
                    }
                    if ($6.tag === "Right") {
                      const $7 = $6._1;
                      return f => f($7);
                    }
                    $runtime.fail();
                  })()(a => Data$dEither.$Either("Right", {...rec, foo: a}));
                });
              }
            }
          }
          $runtime.fail();
        })();
        if ($4.tag === "Left") { return Data$dEither.$Either("Left", $3($4._1)); }
        if ($4.tag === "Right") { return Data$dEither.$Either("Right", $4._1); }
        $runtime.fail();
      };
    })()
  };
  const $2 = Codec$dJson$dUnidirectional$dValue.toJObject(j);
  if ($2.tag === "Left") { return Data$dEither.$Either("Left", $2._1); }
  if ($2.tag === "Right") {
    const $3 = $0.toRecordObj(Type$dProxy.Proxy)($1)($2._1);
    if ($3.tag === "Left") {
      const v2 = $1.nested(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $2._1), "nested");
      if (v2.tag === "Left") {
        return Data$dEither.$Either(
          "Left",
          $3._1.tag === "AccumulateError"
            ? Codec$dJson$dUnidirectional$dValue.$DecodeError("AccumulateError", Data$dList$dTypes.$List("Cons", v2._1, $3._1._1))
            : Codec$dJson$dUnidirectional$dValue.$DecodeError(
                "AccumulateError",
                Data$dList$dTypes.$List("Cons", v2._1, Data$dList$dTypes.$List("Cons", $3._1, Data$dList$dTypes.Nil))
              )
        );
      }
      return Data$dEither.$Either("Left", $3._1);
    }
    if ($3.tag === "Right") {
      return (() => {
        if ($3.tag === "Left") {
          const $4 = $3._1;
          return v => Data$dEither.$Either("Left", $4);
        }
        if ($3.tag === "Right") {
          const $4 = $3._1;
          return f => f($4);
        }
        $runtime.fail();
      })()(rec => {
        const $4 = $1.nested(k => Foreign$dObject._lookup(Data$dMaybe.Nothing, Data$dMaybe.Just, k, $2._1), "nested");
        return (() => {
          if ($4.tag === "Left") {
            const $5 = $4._1;
            return v => Data$dEither.$Either("Left", $5);
          }
          if ($4.tag === "Right") {
            const $5 = $4._1;
            return f => f($5);
          }
          $runtime.fail();
        })()(a => Data$dEither.$Either("Right", {...rec, nested: a}));
      });
    }
  }
  $runtime.fail();
};
export {decoder, test};
