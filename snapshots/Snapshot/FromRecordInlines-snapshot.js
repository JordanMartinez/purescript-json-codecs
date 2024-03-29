// @inline export encoder arity=1
import * as $runtime from "../runtime.js";
import * as Data$dArgonaut$dCore from "../Data.Argonaut.Core/index.js";
import * as Data$dFoldable from "../Data.Foldable/index.js";
import * as Data$dFunctor from "../Data.Functor/index.js";
import * as Data$dInt from "../Data.Int/index.js";
import * as Data$dMaybe from "../Data.Maybe/index.js";
import * as Data$dShow from "../Data.Show/index.js";
import * as Data$dTuple from "../Data.Tuple/index.js";
import * as Foreign$dObject from "../Foreign.Object/index.js";
const encoder = /* #__PURE__ */ (() => {
  const $0 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
  const $1 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
  const $2 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName"));
  const $3 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
  const $4 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
  const $5 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
  return values => Data$dArgonaut$dCore.fromObject((() => {
    const $6 = $3(Data$dArgonaut$dCore.fromString(values["reqRen'"]));
    const $7 = $6._2;
    const $8 = $2(Data$dArgonaut$dCore.fromString(values.reqRen));
    const $9 = $8._2;
    const $10 = $1(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(values["req'"])));
    const $11 = $5(Data$dArgonaut$dCore.fromObject((() => {
      const $11 = $4(Data$dArgonaut$dCore.fromBoolean(values.nested.other));
      const $12 = $11._2;
      const $13 = (() => {
        if ($11._1.tag === "Nothing") { return "other"; }
        if ($11._1.tag === "Just") { return $11._1._1; }
        $runtime.fail();
      })();
      const obj = Foreign$dObject.mutate($14 => () => {
        $14[$13] = $12;
        return $14;
      })(Foreign$dObject.empty);
      const $14 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      if (values.nested.foo.tag === "Just") {
        const $15 = $14(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._2;
        const $16 = (() => {
          if ($14(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1.tag === "Nothing") { return "foo"; }
          if ($14(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1.tag === "Just") { return $14(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($17 => () => {
          $17[$16] = $15;
          return $17;
        })(obj);
      }
      return obj;
    })()));
    const $12 = $11._2;
    const $13 = (() => {
      if ($11._1.tag === "Nothing") { return "nested"; }
      if ($11._1.tag === "Just") { return $11._1._1; }
      $runtime.fail();
    })();
    return Foreign$dObject.mutate($14 => () => {
      $14[$13] = $12;
      return $14;
    })((() => {
      const $14 = $10._2;
      const $15 = $0(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(values.req)));
      const $16 = $15._2;
      const $17 = (() => {
        if ($15._1.tag === "Nothing") { return "req"; }
        if ($15._1.tag === "Just") { return $15._1._1; }
        $runtime.fail();
      })();
      const obj = (() => {
        const obj = (() => {
          const obj = Foreign$dObject.mutate($18 => () => {
            $18[$17] = $16;
            return $18;
          })((() => {
            const $18 = (() => {
              if ($10._1.tag === "Nothing") { return "req'"; }
              if ($10._1.tag === "Just") { return $10._1._1; }
              $runtime.fail();
            })();
            return Foreign$dObject.mutate($19 => () => {
              $19[$18] = $14;
              return $19;
            })((() => {
              const $19 = (() => {
                if ($8._1.tag === "Nothing") { return "reqRen"; }
                if ($8._1.tag === "Just") { return $8._1._1; }
                $runtime.fail();
              })();
              return Foreign$dObject.mutate($20 => () => {
                $20[$19] = $9;
                return $20;
              })((() => {
                const $20 = (() => {
                  if ($6._1.tag === "Nothing") { return "reqRen'"; }
                  if ($6._1.tag === "Just") { return $6._1._1; }
                  $runtime.fail();
                })();
                return Foreign$dObject.mutate($21 => () => {
                  $21[$20] = $7;
                  return $21;
                })(Foreign$dObject.empty);
              })());
            })());
          })());
          const $18 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherNameA"));
          const obj$1 = (() => {
            const v2 = values["optRen'"].tag === "Just" ? Data$dMaybe.$Maybe("Just", $18(Data$dArgonaut$dCore.fromString(values["optRen'"]._1))) : Data$dMaybe.Nothing;
            const obj$1 = (() => {
              if (v2.tag === "Nothing") { return obj; }
              if (v2.tag === "Just") {
                const $19 = v2._1._2;
                const $20 = (() => {
                  if (v2._1._1.tag === "Nothing") { return "optRen'"; }
                  if (v2._1._1.tag === "Just") { return v2._1._1._1; }
                  $runtime.fail();
                })();
                return Foreign$dObject.mutate($21 => () => {
                  $21[$20] = $19;
                  return $21;
                })(obj);
              }
              $runtime.fail();
            })();
            const obj$2 = (() => {
              const $19 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
              const obj$2 = (() => {
                if (values.optRen.tag === "Just") {
                  const $20 = $19(Data$dArgonaut$dCore.fromString(values.optRen._1))._2;
                  const $21 = (() => {
                    if ($19(Data$dArgonaut$dCore.fromString(values.optRen._1))._1.tag === "Nothing") { return "optRen"; }
                    if ($19(Data$dArgonaut$dCore.fromString(values.optRen._1))._1.tag === "Just") { return $19(Data$dArgonaut$dCore.fromString(values.optRen._1))._1._1; }
                    $runtime.fail();
                  })();
                  return Foreign$dObject.mutate($22 => () => {
                    $22[$21] = $20;
                    return $22;
                  })(obj$1);
                }
                return obj$1;
              })();
              const obj$3 = (() => {
                if (values["optAssocArr'"].length === 0) { return obj$2; }
                const $20 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
                  const $20 = Data$dShow.showStringImpl(v._1);
                  const $21 = Data$dArgonaut$dCore.fromString(v._2);
                  return Foreign$dObject.mutate($22 => () => {
                    $22[$20] = $21;
                    return $22;
                  })(acc);
                })(Foreign$dObject.empty)(values["optAssocArr'"]));
                return Foreign$dObject.mutate($21 => () => {
                  $21["optAssocArr'"] = $20;
                  return $21;
                })(obj$2);
              })();
              if (values.optAssocArr.length === 0) { return obj$3; }
              const $20 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
                const $20 = Data$dShow.showStringImpl(v._1);
                const $21 = Data$dArgonaut$dCore.fromString(v._2);
                return Foreign$dObject.mutate($22 => () => {
                  $22[$20] = $21;
                  return $22;
                })(acc);
              })(Foreign$dObject.empty)(values.optAssocArr));
              return Foreign$dObject.mutate($21 => () => {
                $21.optAssocArr = $20;
                return $21;
              })(obj$3);
            })();
            if (values["optArr'"].length === 0) { return obj$2; }
            const $19 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(values["optArr'"]));
            return Foreign$dObject.mutate($20 => () => {
              $20["optArr'"] = $19;
              return $20;
            })(obj$2);
          })();
          if (values.optArr.length === 0) { return obj$1; }
          const $19 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(values.optArr));
          return Foreign$dObject.mutate($20 => () => {
            $20.optArr = $19;
            return $20;
          })(obj$1);
        })();
        const $18 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
        if (values["opt'"].tag === "Just") {
          const $19 = $18(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._2;
          const $20 = (() => {
            if ($18(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1.tag === "Nothing") { return "opt'"; }
            if ($18(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1.tag === "Just") { return $18(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1._1; }
            $runtime.fail();
          })();
          return Foreign$dObject.mutate($21 => () => {
            $21[$20] = $19;
            return $21;
          })(obj);
        }
        return obj;
      })();
      const $18 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      if (values.opt.tag === "Just") {
        const $19 = $18(Data$dArgonaut$dCore.fromString(values.opt._1))._2;
        const $20 = (() => {
          if ($18(Data$dArgonaut$dCore.fromString(values.opt._1))._1.tag === "Nothing") { return "opt"; }
          if ($18(Data$dArgonaut$dCore.fromString(values.opt._1))._1.tag === "Just") { return $18(Data$dArgonaut$dCore.fromString(values.opt._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($21 => () => {
          $21[$20] = $19;
          return $21;
        })(obj);
      }
      return obj;
    })());
  })());
})();
export {encoder};
