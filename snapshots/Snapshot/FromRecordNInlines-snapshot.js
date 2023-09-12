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
const Foo = x => x;
const newtypeFoo_ = {Coercible0: () => {}};
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
    const $8 = $0(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(values.req)));
    const $9 = $5(Data$dArgonaut$dCore.fromObject((() => {
      const $9 = $4(Data$dArgonaut$dCore.fromBoolean(values.nested.other));
      const $10 = $9._2;
      const $11 = (() => {
        if ($9._1.tag === "Nothing") { return "other"; }
        if ($9._1.tag === "Just") { return $9._1._1; }
        $runtime.fail();
      })();
      const obj = Foreign$dObject.mutate($12 => () => {
        $12[$11] = $10;
        return $12;
      })(Foreign$dObject.empty);
      const $12 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      if (values.nested.foo.tag === "Just") {
        const $13 = $12(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._2;
        const $14 = (() => {
          if ($12(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1.tag === "Nothing") { return "foo"; }
          if ($12(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1.tag === "Just") { return $12(Data$dArgonaut$dCore.fromBoolean(values.nested.foo._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($15 => () => {
          $15[$14] = $13;
          return $15;
        })(obj);
      }
      return obj;
    })()));
    const $10 = $9._2;
    const $11 = (() => {
      if ($9._1.tag === "Nothing") { return "nested"; }
      if ($9._1.tag === "Just") { return $9._1._1; }
      $runtime.fail();
    })();
    return Foreign$dObject.mutate($12 => () => {
      $12[$11] = $10;
      return $12;
    })((() => {
      const $12 = $8._2;
      const $13 = (() => {
        if ($8._1.tag === "Nothing") { return "req"; }
        if ($8._1.tag === "Just") { return $8._1._1; }
        $runtime.fail();
      })();
      const obj = (() => {
        const obj = (() => {
          const obj = (() => {
            const obj = (() => {
              const obj = (() => {
                const obj = (() => {
                  const obj = (() => {
                    const obj = Foreign$dObject.mutate($14 => () => {
                      $14[$13] = $12;
                      return $14;
                    })((() => {
                      const $14 = $2(Data$dArgonaut$dCore.fromString(values.reqRen));
                      const $15 = $14._2;
                      const $16 = $1(Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(values["req'"])));
                      const $17 = $16._2;
                      const $18 = (() => {
                        if ($16._1.tag === "Nothing") { return "req'"; }
                        if ($16._1.tag === "Just") { return $16._1._1; }
                        $runtime.fail();
                      })();
                      return Foreign$dObject.mutate($19 => () => {
                        $19[$18] = $17;
                        return $19;
                      })((() => {
                        const $19 = (() => {
                          if ($14._1.tag === "Nothing") { return "reqRen"; }
                          if ($14._1.tag === "Just") { return $14._1._1; }
                          $runtime.fail();
                        })();
                        return Foreign$dObject.mutate($20 => () => {
                          $20[$19] = $15;
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
                    const $14 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherNameA"));
                    if (values["optRen'"].tag === "Just") {
                      const $15 = $14(Data$dArgonaut$dCore.fromString(values["optRen'"]._1))._2;
                      const $16 = (() => {
                        if ($14(Data$dArgonaut$dCore.fromString(values["optRen'"]._1))._1.tag === "Nothing") { return "optRen'"; }
                        if ($14(Data$dArgonaut$dCore.fromString(values["optRen'"]._1))._1.tag === "Just") {
                          return $14(Data$dArgonaut$dCore.fromString(values["optRen'"]._1))._1._1;
                        }
                        $runtime.fail();
                      })();
                      return Foreign$dObject.mutate($17 => () => {
                        $17[$16] = $15;
                        return $17;
                      })(obj);
                    }
                    return obj;
                  })();
                  const $14 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
                  if (values.optRen.tag === "Just") {
                    const $15 = $14(Data$dArgonaut$dCore.fromString(values.optRen._1))._2;
                    const $16 = (() => {
                      if ($14(Data$dArgonaut$dCore.fromString(values.optRen._1))._1.tag === "Nothing") { return "optRen"; }
                      if ($14(Data$dArgonaut$dCore.fromString(values.optRen._1))._1.tag === "Just") { return $14(Data$dArgonaut$dCore.fromString(values.optRen._1))._1._1; }
                      $runtime.fail();
                    })();
                    return Foreign$dObject.mutate($17 => () => {
                      $17[$16] = $15;
                      return $17;
                    })(obj);
                  }
                  return obj;
                })();
                if (values["optAssocArr'"].length === 0) { return obj; }
                const $14 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
                  const $14 = Data$dShow.showStringImpl(v._1);
                  const $15 = Data$dArgonaut$dCore.fromString(v._2);
                  return Foreign$dObject.mutate($16 => () => {
                    $16[$14] = $15;
                    return $16;
                  })(acc);
                })(Foreign$dObject.empty)(values["optAssocArr'"]));
                return Foreign$dObject.mutate($15 => () => {
                  $15["optAssocArr'"] = $14;
                  return $15;
                })(obj);
              })();
              if (values.optAssocArr.length === 0) { return obj; }
              const $14 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
                const $14 = Data$dShow.showStringImpl(v._1);
                const $15 = Data$dArgonaut$dCore.fromString(v._2);
                return Foreign$dObject.mutate($16 => () => {
                  $16[$14] = $15;
                  return $16;
                })(acc);
              })(Foreign$dObject.empty)(values.optAssocArr));
              return Foreign$dObject.mutate($15 => () => {
                $15.optAssocArr = $14;
                return $15;
              })(obj);
            })();
            if (values["optArr'"].length === 0) { return obj; }
            const $14 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(values["optArr'"]));
            return Foreign$dObject.mutate($15 => () => {
              $15["optArr'"] = $14;
              return $15;
            })(obj);
          })();
          if (values.optArr.length === 0) { return obj; }
          const $14 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(values.optArr));
          return Foreign$dObject.mutate($15 => () => {
            $15.optArr = $14;
            return $15;
          })(obj);
        })();
        const $14 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
        if (values["opt'"].tag === "Just") {
          const $15 = $14(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._2;
          const $16 = (() => {
            if ($14(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1.tag === "Nothing") { return "opt'"; }
            if ($14(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1.tag === "Just") { return $14(Data$dArgonaut$dCore.fromString(values["opt'"]._1))._1._1; }
            $runtime.fail();
          })();
          return Foreign$dObject.mutate($17 => () => {
            $17[$16] = $15;
            return $17;
          })(obj);
        }
        return obj;
      })();
      const $14 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      if (values.opt.tag === "Just") {
        const $15 = $14(Data$dArgonaut$dCore.fromString(values.opt._1))._2;
        const $16 = (() => {
          if ($14(Data$dArgonaut$dCore.fromString(values.opt._1))._1.tag === "Nothing") { return "opt"; }
          if ($14(Data$dArgonaut$dCore.fromString(values.opt._1))._1.tag === "Just") { return $14(Data$dArgonaut$dCore.fromString(values.opt._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($17 => () => {
          $17[$16] = $15;
          return $17;
        })(obj);
      }
      return obj;
    })());
  })());
})();
const test = j => Data$dArgonaut$dCore.fromObject((() => {
  const $0 = Data$dArgonaut$dCore.fromString(j["reqRen'"]);
  const $1 = Data$dArgonaut$dCore.fromObject((() => {
    const $1 = Data$dArgonaut$dCore.fromBoolean(j.nested.other);
    const obj = Foreign$dObject.mutate($2 => () => {
      $2.other = $1;
      return $2;
    })(Foreign$dObject.empty);
    const $2 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
    if (j.nested.foo.tag === "Just") {
      const $3 = $2(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._2;
      const $4 = (() => {
        if ($2(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1.tag === "Nothing") { return "foo"; }
        if ($2(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1.tag === "Just") { return $2(Data$dArgonaut$dCore.fromBoolean(j.nested.foo._1))._1._1; }
        $runtime.fail();
      })();
      return Foreign$dObject.mutate($5 => () => {
        $5[$4] = $3;
        return $5;
      })(obj);
    }
    return obj;
  })());
  return Foreign$dObject.mutate($2 => () => {
    $2.nested = $1;
    return $2;
  })((() => {
    const $2 = Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(j.req));
    const obj = Foreign$dObject.mutate($3 => () => {
      $3.req = $2;
      return $3;
    })((() => {
      const $3 = Data$dArgonaut$dCore.fromString(j.reqRen);
      const $4 = Data$dArgonaut$dCore.fromNumber(Data$dInt.toNumber(j["req'"]));
      return Foreign$dObject.mutate($5 => () => {
        $5["req'"] = $4;
        return $5;
      })(Foreign$dObject.mutate($5 => () => {
        $5.otherName = $3;
        return $5;
      })(Foreign$dObject.mutate($5 => () => {
        $5.otherName2 = $0;
        return $5;
      })(Foreign$dObject.empty)));
    })());
    const $3 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherNameA"));
    const obj$1 = (() => {
      if (j["optRen'"].tag === "Just") {
        const $4 = $3(Data$dArgonaut$dCore.fromString(j["optRen'"]._1))._2;
        const $5 = (() => {
          if ($3(Data$dArgonaut$dCore.fromString(j["optRen'"]._1))._1.tag === "Nothing") { return "optRen'"; }
          if ($3(Data$dArgonaut$dCore.fromString(j["optRen'"]._1))._1.tag === "Just") { return $3(Data$dArgonaut$dCore.fromString(j["optRen'"]._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($6 => () => {
          $6[$5] = $4;
          return $6;
        })(obj);
      }
      return obj;
    })();
    const $4 = Data$dTuple.Tuple(Data$dMaybe.$Maybe("Just", "otherName2"));
    const obj$2 = (() => {
      if (j.optRen.tag === "Just") {
        const $5 = $4(Data$dArgonaut$dCore.fromString(j.optRen._1))._2;
        const $6 = (() => {
          if ($4(Data$dArgonaut$dCore.fromString(j.optRen._1))._1.tag === "Nothing") { return "optRen"; }
          if ($4(Data$dArgonaut$dCore.fromString(j.optRen._1))._1.tag === "Just") { return $4(Data$dArgonaut$dCore.fromString(j.optRen._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($7 => () => {
          $7[$6] = $5;
          return $7;
        })(obj$1);
      }
      return obj$1;
    })();
    const obj$3 = (() => {
      if (j["optAssocArr'"].length === 0) { return obj$2; }
      const $5 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
        const $5 = Data$dShow.showStringImpl(v._1);
        const $6 = Data$dArgonaut$dCore.fromString(v._2);
        return Foreign$dObject.mutate($7 => () => {
          $7[$5] = $6;
          return $7;
        })(acc);
      })(Foreign$dObject.empty)(j["optAssocArr'"]));
      return Foreign$dObject.mutate($6 => () => {
        $6["optAssocArr'"] = $5;
        return $6;
      })(obj$2);
    })();
    const obj$4 = (() => {
      if (j.optAssocArr.length === 0) { return obj$3; }
      const $5 = Data$dArgonaut$dCore.fromObject(Data$dFoldable.foldlArray(acc => v => {
        const $5 = Data$dShow.showStringImpl(v._1);
        const $6 = Data$dArgonaut$dCore.fromString(v._2);
        return Foreign$dObject.mutate($7 => () => {
          $7[$5] = $6;
          return $7;
        })(acc);
      })(Foreign$dObject.empty)(j.optAssocArr));
      return Foreign$dObject.mutate($6 => () => {
        $6.optAssocArr = $5;
        return $6;
      })(obj$3);
    })();
    const obj$5 = (() => {
      if (j["optArr'"].length === 0) { return obj$4; }
      const $5 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(j["optArr'"]));
      return Foreign$dObject.mutate($6 => () => {
        $6["optArr'"] = $5;
        return $6;
      })(obj$4);
    })();
    const obj$6 = (() => {
      const obj$6 = (() => {
        if (j.optArr.length === 0) { return obj$5; }
        const $5 = Data$dArgonaut$dCore.fromArray(Data$dFunctor.arrayMap(Data$dArgonaut$dCore.fromString)(j.optArr));
        return Foreign$dObject.mutate($6 => () => {
          $6.optArr = $5;
          return $6;
        })(obj$5);
      })();
      const $5 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
      if (j["opt'"].tag === "Just") {
        const $6 = $5(Data$dArgonaut$dCore.fromString(j["opt'"]._1))._2;
        const $7 = (() => {
          if ($5(Data$dArgonaut$dCore.fromString(j["opt'"]._1))._1.tag === "Nothing") { return "opt'"; }
          if ($5(Data$dArgonaut$dCore.fromString(j["opt'"]._1))._1.tag === "Just") { return $5(Data$dArgonaut$dCore.fromString(j["opt'"]._1))._1._1; }
          $runtime.fail();
        })();
        return Foreign$dObject.mutate($8 => () => {
          $8[$7] = $6;
          return $8;
        })(obj$6);
      }
      return obj$6;
    })();
    const $5 = Data$dTuple.Tuple(Data$dMaybe.Nothing);
    if (j.opt.tag === "Just") {
      const $6 = $5(Data$dArgonaut$dCore.fromString(j.opt._1))._2;
      const $7 = (() => {
        if ($5(Data$dArgonaut$dCore.fromString(j.opt._1))._1.tag === "Nothing") { return "opt"; }
        if ($5(Data$dArgonaut$dCore.fromString(j.opt._1))._1.tag === "Just") { return $5(Data$dArgonaut$dCore.fromString(j.opt._1))._1._1; }
        $runtime.fail();
      })();
      return Foreign$dObject.mutate($8 => () => {
        $8[$7] = $6;
        return $8;
      })(obj$6);
    }
    return obj$6;
  })());
})());
export {Foo, encoder, newtypeFoo_, test};
