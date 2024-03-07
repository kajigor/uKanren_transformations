module Prop_online where

import Stream
import Control.Monad
import Term

evaloI x0 = Immature $ msum [do {let {x4 = Trueo};
                      x5 <- case x0 of
                            {Lit y5 -> return y5; _ -> mzero};
                      guard (x5 == x4);
                      return ()},
                  do {x1 <- case x0 of
                            {Var y1 -> return y1; _ -> mzero};
                      elemoI x1;
                      return ()},
                  do {x2 <- case x0 of
                            {Neg y2 -> return y2; _ -> mzero};
                      _evaloI x2;
                      return ()},
                  do {(x2, x3) <- case x0 of
                                  {Disj y2 y3 -> return (y2, y3); _ -> mzero};
                      oroEvaloEvaloII x2 x3;
                      return ()},
                  do {(x2, x3) <- case x0 of
                                  {Conj y2 y3 -> return (y2, y3); _ -> mzero};
                      evaloI x2;
                      evaloI x3;
                      return ()},
                  do {(x2, x3) <- case x0 of
                                  {Impl y2 y3 -> return (y2, y3); _ -> mzero};
                      implicationoEvaloEvaloII x2 x3;
                      return ()}]
_evaloI x0 = Immature $ msum [do {let {x6 = Falso};
                       x7 <- case x0 of
                             {Lit y7 -> return y7; _ -> mzero};
                       guard (x7 == x6);
                       return ()},
                   do {x1 <- case x0 of
                             {Neg y1 -> return y1; _ -> mzero};
                       evaloI x1;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                       _evaloI x1;
                       _evaloI x2;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                       andoEvaloEvaloII x1 x2;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                       evaloI x1;
                       _evaloI x2;
                       return ()}]
andoEvaloEvaloII x0 x1 = Immature $ msum [do {_evaloI x0;
                                   evaloI x1;
                                   return ()},
                               do {evaloI x0; _evaloI x1; return ()},
                               do {_evaloI x0; _evaloI x1; return ()}]
elemoI x0 = Immature $ msum [do {guard (x0 == Zero); return ()},
                  do {x1 <- case x0 of
                            {Succ y1 -> return y1; _ -> mzero};
                      _elemoI x1;
                      return ()}]
_elemoI x0 = Immature $ msum [do {guard (x0 == Zero); return ()},
                   do {x1 <- case x0 of
                             {Succ y1 -> return y1; _ -> mzero};
                       __elemoI x1;
                       return ()}]
__elemoI x0 = Immature $ msum [do {guard (x0 == Zero); return ()}]
evaloO = Immature $ msum [do {let {x4 = Trueo};
                   let {x5 = x4};
                   let {x0 = Lit x5};
                   return x0},
               do {x1 <- elemoO; let {x0 = Var x1}; return x0},
               do {x2 <- _evaloO; let {x0 = Neg x2}; return x0},
               do {(x2, x3) <- oroEvaloEvaloOO; let {x0 = Disj x2 x3}; return x0},
               do {x2 <- evaloO; x3 <- evaloO; let {x0 = Conj x2 x3}; return x0},
               do {(x2, x3) <- implicationoEvaloEvaloOO;
                   let {x0 = Impl x2 x3};
                   return x0}]
_evaloO = Immature $ msum [do {let {x6 = Falso};
                    let {x7 = x6};
                    let {x0 = Lit x7};
                    return x0},
                do {x1 <- evaloO; let {x0 = Neg x1}; return x0},
                do {x1 <- _evaloO;
                    x2 <- _evaloO;
                    let {x0 = Disj x1 x2};
                    return x0},
                do {(x1, x2) <- andoEvaloEvaloOO;
                    let {x0 = Conj x1 x2};
                    return x0},
                do {x1 <- evaloO; x2 <- _evaloO; let {x0 = Impl x1 x2}; return x0}]
andoEvaloEvaloOO = Immature $ msum [do {x0 <- _evaloO;
                             x1 <- evaloO;
                             return (x0, x1)},
                         do {x0 <- evaloO; x1 <- _evaloO; return (x0, x1)},
                         do {x0 <- _evaloO; x1 <- _evaloO; return (x0, x1)}]
elemoO = Immature $ msum [do {let {x0 = Zero}; return x0},
               do {x1 <- _elemoO; let {x0 = Succ x1}; return x0}]
_elemoO = Immature $ msum [do {let {x0 = Zero}; return x0},
                do {x1 <- __elemoO; let {x0 = Succ x1}; return x0}]
__elemoO = Immature $ msum [do {let {x0 = Zero}; return x0}]
implicationoEvaloEvaloII x0 x1 = Immature $ msum [do {_evaloI x0;
                                           evaloI x1;
                                           return ()},
                                       do {_evaloI x0; _evaloI x1; return ()},
                                       do {evaloI x0; evaloI x1; return ()}]
implicationoEvaloEvaloOO = Immature $ msum [do {x0 <- _evaloO;
                                     x1 <- evaloO;
                                     return (x0, x1)},
                                 do {x0 <- _evaloO; x1 <- _evaloO; return (x0, x1)},
                                 do {x0 <- evaloO; x1 <- evaloO; return (x0, x1)}]
oroEvaloEvaloII x0 x1 = Immature $ msum [do {evaloI x0; evaloI x1; return ()},
                              do {_evaloI x0; evaloI x1; return ()},
                              do {evaloI x0; _evaloI x1; return ()}]
oroEvaloEvaloOO = Immature $ msum [do {x0 <- evaloO;
                            x1 <- evaloO;
                            return (x0, x1)},
                        do {x0 <- _evaloO; x1 <- evaloO; return (x0, x1)},
                        do {x0 <- evaloO; x1 <- _evaloO; return (x0, x1)}]