module Contains1_online where

import Stream
import Control.Monad
import Term

containsoI x0 = Immature $ msum [do {let {x6 = O};
                          let {x5 = S x6};
                          (x8, x9) <- case x0 of
                                      {Cons y8 y9 -> return (y8, y9); _ -> mzero};
                          guard (x8 == x5);
                          let {x7 = x9};
                          (x1, x2) <- case x7 of
                                      {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                          newoConoII x1 x2;
                          return ()},
                      do {(x3, x4) <- case x0 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          _conoI x4;
                          return ()}]
_conoI x0 = Immature $ msum [do {let {x16 = O};
                      let {x15 = S x16};
                      (x18, x19) <- case x0 of
                                    {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                      guard (x18 == x15);
                      let {x17 = x19};
                      (x1, x2) <- case x17 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      newoConoII x1 x2;
                      return ()},
                  do {(x3, x4) <- case x0 of
                                  {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                      _conoI x4;
                      return ()}]
containsoO gen__conoO_x17 gen__conoO_x3 gen_containsoO_x3 gen_containsoO_x7 = Immature $ msum [do {let {x6 = O};
                                                                                        let {x5 = S x6};
                                                                                        let {x8 = x5};
                                                                                        (x9,
                                                                                         x7) <- do {x7 <- gen_containsoO_x7;
                                                                                                    return (x7,
                                                                                                            x7)};
                                                                                        let {x0 = Cons x8 x9};
                                                                                        (x1,
                                                                                         x2) <- case x7 of
                                                                                                {Cons y1
                                                                                                      y2 -> return (y1,
                                                                                                                    y2);
                                                                                                 _ -> mzero};
                                                                                        newoConoII x1 x2;
                                                                                        return x0},
                                                                                    do {x4 <- _conoO gen__conoO_x17 gen__conoO_x3;
                                                                                        (x0,
                                                                                         x3) <- do {x3 <- gen_containsoO_x3;
                                                                                                    let {x0 = Cons x3 x4};
                                                                                                    return (x0,
                                                                                                            x3)};
                                                                                        return x0}]
_conoO gen__conoO_x17 gen__conoO_x3 = Immature $ msum [do {let {x16 = O};
                                                let {x15 = S x16};
                                                let {x18 = x15};
                                                (x19, x17) <- do {x17 <- gen__conoO_x17;
                                                                  return (x17, x17)};
                                                let {x0 = Cons x18 x19};
                                                (x1, x2) <- case x17 of
                                                            {Cons y1 y2 -> return (y1, y2);
                                                             _ -> mzero};
                                                newoConoII x1 x2;
                                                return x0},
                                            do {x4 <- _conoO gen__conoO_x17 gen__conoO_x3;
                                                (x0, x3) <- do {x3 <- gen__conoO_x3;
                                                                let {x0 = Cons x3 x4};
                                                                return (x0, x3)};
                                                return x0}]
newoConoII x0 x1 = Immature $ msum [do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             _newoConoII x2 x3;
                             guard (x0 == O);
                             return ()},
                         do {__appendo2Appendo2Appendo1ConoII x0 x1; return ()}]
__appendo2Appendo2Appendo1ConoII x0 x1 = Immature $ msum [do {_conoI x1;
                                                   return ()},
                                               do {appendo2I x0;
                                                   (x2, x3) <- case x1 of
                                                               {Cons y2 y3 -> return (y2, y3);
                                                                _ -> mzero};
                                                   newoConoII x2 x3;
                                                   return ()}]
_newoConoII x0 x1 = Immature $ msum [do {let {x11 = O};
                              let {x10 = S x11};
                              x12 <- case x0 of
                                     {S y12 -> return y12; _ -> mzero};
                              guard (x12 == x10);
                              return ()},
                          do {appendo2Appendo2Appendo1ConoII x0 x1; return ()}]
appendo2I x0 = Immature $ msum [do {let {x13 = O};
                         x14 <- case x0 of
                                {S y14 -> return y14; _ -> mzero};
                         guard (x14 == x13);
                         return ()}]
appendo2Appendo2Appendo1ConoII x0 x1 = Immature $ msum [do {_conoI x1;
                                                 return ()},
                                             do {_appendo2Appendo2Appendo1ConoII x0 x1; return ()}]
_appendo2Appendo2Appendo1ConoII x0 x1 = Immature $ msum [do {appendo2I x0;
                                                  (x2, x3) <- case x1 of
                                                              {Cons y2 y3 -> return (y2, y3);
                                                               _ -> mzero};
                                                  newoConoII x2 x3;
                                                  return ()}]