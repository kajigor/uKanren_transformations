module Prop where

import Stream
import Control.Monad

data Term
    = Conj Term Term
    | Disj Term Term
    | Falso
    | Impl Term Term
    | Lit Term
    | Neg Term
    | Trueo
    deriving (Show, Eq)
evaloI x0 = msum [do {let {x6 = Trueo};
                      ___evaloEvaloII x0 x6;
                      return ()},
                  do {let {x7 = Trueo}; __evaloEvaloII x0 x7; return ()},
                  do {let {x8 = Falso}; ___evaloEvaloII x0 x8; return ()}]
___evaloEvaloII x0 x1 = msum [do {let {x11 = Trueo};
                                  guard (x1 == Trueo);
                                  x12 <- case x0 of
                                         {Lit y12 -> return y12; _ -> mzero};
                                  guard (x12 == x11);
                                  return ()},
                              do {x2 <- case x0 of
                                        {Neg y2 -> return y2; _ -> mzero};
                                  x3 <- __evaloEvaloIO x2;
                                  notoII x1 x3;
                                  return ()},
                              do {(x2, x4) <- case x0 of
                                              {Disj y2 y4 -> return (y2, y4); _ -> mzero};
                                  (x3, x5) <- oroEvaloEvaloEvaloEvaloIIOO x2 x4;
                                  oroIII x1 x3 x5;
                                  return ()},
                              do {(x2, x4) <- case x0 of
                                              {Conj y2 y4 -> return (y2, y4); _ -> mzero};
                                  x3 <- ___evaloEvaloIO x2;
                                  x5 <- ___evaloEvaloIO x4;
                                  andoIII x1 x3 x5;
                                  return ()},
                              do {(x2, x4) <- case x0 of
                                              {Impl y2 y4 -> return (y2, y4); _ -> mzero};
                                  (x3, x5) <- implicationoEvaloEvaloEvaloEvaloIIOO x2 x4;
                                  implicationoIII x1 x3 x5;
                                  return ()}]
___evaloEvaloIO x0 = msum [do {let {x1 = Trueo};
                               let {x11 = Trueo};
                               x12 <- case x0 of
                                      {Lit y12 -> return y12; _ -> mzero};
                               guard (x12 == x11);
                               return x1},
                           do {x2 <- case x0 of
                                     {Neg y2 -> return y2; _ -> mzero};
                               x3 <- __evaloEvaloIO x2;
                               x1 <- notoOI x3;
                               return x1},
                           do {(x2, x4) <- case x0 of
                                           {Disj y2 y4 -> return (y2, y4); _ -> mzero};
                               (x3, x5) <- oroEvaloEvaloEvaloEvaloIIOO x2 x4;
                               x1 <- oroOII x3 x5;
                               return x1},
                           do {(x2, x4) <- case x0 of
                                           {Conj y2 y4 -> return (y2, y4); _ -> mzero};
                               x3 <- ___evaloEvaloIO x2;
                               x5 <- ___evaloEvaloIO x4;
                               x1 <- andoOII x3 x5;
                               return x1},
                           do {(x2, x4) <- case x0 of
                                           {Impl y2 y4 -> return (y2, y4); _ -> mzero};
                               (x3, x5) <- implicationoEvaloEvaloEvaloEvaloIIOO x2 x4;
                               x1 <- implicationoOII x3 x5;
                               return x1}]
__evaloEvaloII x0 x1 = msum [do {let {x9 = Falso};
                                 guard (x1 == Falso);
                                 x10 <- case x0 of
                                        {Lit y10 -> return y10; _ -> mzero};
                                 guard (x10 == x9);
                                 return ()},
                             do {x2 <- case x0 of
                                       {Neg y2 -> return y2; _ -> mzero};
                                 x3 <- ___evaloEvaloIO x2;
                                 notoII x1 x3;
                                 return ()},
                             do {(x2, x4) <- case x0 of
                                             {Disj y2 y4 -> return (y2, y4); _ -> mzero};
                                 x3 <- __evaloEvaloIO x2;
                                 x5 <- __evaloEvaloIO x4;
                                 oroIII x1 x3 x5;
                                 return ()},
                             do {(x2, x4) <- case x0 of
                                             {Conj y2 y4 -> return (y2, y4); _ -> mzero};
                                 (x3, x5) <- andoEvaloEvaloEvaloEvaloIIOO x2 x4;
                                 andoIII x1 x3 x5;
                                 return ()},
                             do {(x2, x4) <- case x0 of
                                             {Impl y2 y4 -> return (y2, y4); _ -> mzero};
                                 x3 <- ___evaloEvaloIO x2;
                                 x5 <- __evaloEvaloIO x4;
                                 implicationoIII x1 x3 x5;
                                 return ()}]
__evaloEvaloIO x0 = msum [do {let {x1 = Falso};
                              let {x9 = Falso};
                              x10 <- case x0 of
                                     {Lit y10 -> return y10; _ -> mzero};
                              guard (x10 == x9);
                              return x1},
                          do {x2 <- case x0 of
                                    {Neg y2 -> return y2; _ -> mzero};
                              x3 <- ___evaloEvaloIO x2;
                              x1 <- notoOI x3;
                              return x1},
                          do {(x2, x4) <- case x0 of
                                          {Disj y2 y4 -> return (y2, y4); _ -> mzero};
                              x3 <- __evaloEvaloIO x2;
                              x5 <- __evaloEvaloIO x4;
                              x1 <- oroOII x3 x5;
                              return x1},
                          do {(x2, x4) <- case x0 of
                                          {Conj y2 y4 -> return (y2, y4); _ -> mzero};
                              (x3, x5) <- andoEvaloEvaloEvaloEvaloIIOO x2 x4;
                              x1 <- andoOII x3 x5;
                              return x1},
                          do {(x2, x4) <- case x0 of
                                          {Impl y2 y4 -> return (y2, y4); _ -> mzero};
                              x3 <- ___evaloEvaloIO x2;
                              x5 <- __evaloEvaloIO x4;
                              x1 <- implicationoOII x3 x5;
                              return x1}]
andoIII x0 x1 x2 = msum [do {guard (x2 == Trueo);
                             guard (x1 == Trueo);
                             guard (x0 == Trueo);
                             return ()},
                         do {guard (x2 == Trueo);
                             guard (x1 == Falso);
                             guard (x0 == Falso);
                             return ()},
                         do {guard (x2 == Falso);
                             guard (x1 == Trueo);
                             guard (x0 == Falso);
                             return ()},
                         do {guard (x2 == Falso);
                             guard (x1 == Falso);
                             guard (x0 == Falso);
                             return ()}]
andoOII x1 x2 = msum [do {let {x0 = Trueo};
                          guard (x2 == Trueo);
                          guard (x1 == Trueo);
                          return x0},
                      do {let {x0 = Falso};
                          guard (x2 == Trueo);
                          guard (x1 == Falso);
                          return x0},
                      do {let {x0 = Falso};
                          guard (x2 == Falso);
                          guard (x1 == Trueo);
                          return x0},
                      do {let {x0 = Falso};
                          guard (x2 == Falso);
                          guard (x1 == Falso);
                          return x0}]
andoEvaloEvaloEvaloEvaloIIOO x0 x1 = msum [do {x2 <- __evaloEvaloIO x0;
                                               x3 <- ___evaloEvaloIO x1;
                                               return (x2, x3)},
                                           do {x2 <- ___evaloEvaloIO x0;
                                               x3 <- __evaloEvaloIO x1;
                                               return (x2, x3)},
                                           do {x2 <- __evaloEvaloIO x0;
                                               x3 <- __evaloEvaloIO x1;
                                               return (x2, x3)}]
evaloO = msum [do {let {x6 = Trueo};
                   x0 <- ___evaloEvaloOI x6;
                   return x0},
               do {let {x7 = Trueo}; x0 <- __evaloEvaloOI x7; return x0},
               do {let {x8 = Falso}; x0 <- ___evaloEvaloOI x8; return x0}]
___evaloEvaloOI x1 = msum [do {let {x11 = Trueo};
                               guard (x1 == Trueo);
                               let {x12 = x11};
                               let {x0 = Lit x12};
                               return x0},
                           do {x3 <- notoIO x1;
                               x2 <- __evaloEvaloOI x3;
                               let {x0 = Neg x2};
                               return x0},
                           do {(x3, x5) <- oroIOO x1;
                               (x2, x4) <- oroEvaloEvaloEvaloEvaloOOII x3 x5;
                               let {x0 = Disj x2 x4};
                               return x0},
                           do {(x3, x5) <- andoIOO x1;
                               x2 <- ___evaloEvaloOI x3;
                               x4 <- ___evaloEvaloOI x5;
                               let {x0 = Conj x2 x4};
                               return x0},
                           do {(x3, x5) <- implicationoIOO x1;
                               (x2, x4) <- implicationoEvaloEvaloEvaloEvaloOOII x3 x5;
                               let {x0 = Impl x2 x4};
                               return x0}]
__evaloEvaloOI x1 = msum [do {let {x9 = Falso};
                              guard (x1 == Falso);
                              let {x10 = x9};
                              let {x0 = Lit x10};
                              return x0},
                          do {x3 <- notoIO x1;
                              x2 <- ___evaloEvaloOI x3;
                              let {x0 = Neg x2};
                              return x0},
                          do {(x3, x5) <- oroIOO x1;
                              x2 <- __evaloEvaloOI x3;
                              x4 <- __evaloEvaloOI x5;
                              let {x0 = Disj x2 x4};
                              return x0},
                          do {(x3, x5) <- andoIOO x1;
                              (x2, x4) <- andoEvaloEvaloEvaloEvaloOOII x3 x5;
                              let {x0 = Conj x2 x4};
                              return x0},
                          do {(x3, x5) <- implicationoIOO x1;
                              x2 <- ___evaloEvaloOI x3;
                              x4 <- __evaloEvaloOI x5;
                              let {x0 = Impl x2 x4};
                              return x0}]
andoIOO x0 = msum [do {let {x2 = Trueo};
                       let {x1 = Trueo};
                       guard (x0 == Trueo);
                       return (x1, x2)},
                   do {let {x2 = Trueo};
                       let {x1 = Falso};
                       guard (x0 == Falso);
                       return (x1, x2)},
                   do {let {x2 = Falso};
                       let {x1 = Trueo};
                       guard (x0 == Falso);
                       return (x1, x2)},
                   do {let {x2 = Falso};
                       let {x1 = Falso};
                       guard (x0 == Falso);
                       return (x1, x2)}]
andoEvaloEvaloEvaloEvaloOOII x2 x3 = msum [do {x0 <- __evaloEvaloOI x2;
                                               x1 <- ___evaloEvaloOI x3;
                                               return (x0, x1)},
                                           do {x0 <- ___evaloEvaloOI x2;
                                               x1 <- __evaloEvaloOI x3;
                                               return (x0, x1)},
                                           do {x0 <- __evaloEvaloOI x2;
                                               x1 <- __evaloEvaloOI x3;
                                               return (x0, x1)}]
implicationoIII x0 x1 x2 = msum [do {guard (x2 == Trueo);
                                     guard (x1 == Falso);
                                     guard (x0 == Trueo);
                                     return ()},
                                 do {guard (x2 == Falso);
                                     guard (x1 == Falso);
                                     guard (x0 == Trueo);
                                     return ()},
                                 do {guard (x2 == Trueo);
                                     guard (x1 == Trueo);
                                     guard (x0 == Trueo);
                                     return ()},
                                 do {guard (x2 == Falso);
                                     guard (x1 == Trueo);
                                     guard (x0 == Falso);
                                     return ()}]
implicationoIOO x0 = msum [do {let {x2 = Trueo};
                               let {x1 = Falso};
                               guard (x0 == Trueo);
                               return (x1, x2)},
                           do {let {x2 = Falso};
                               let {x1 = Falso};
                               guard (x0 == Trueo);
                               return (x1, x2)},
                           do {let {x2 = Trueo};
                               let {x1 = Trueo};
                               guard (x0 == Trueo);
                               return (x1, x2)},
                           do {let {x2 = Falso};
                               let {x1 = Trueo};
                               guard (x0 == Falso);
                               return (x1, x2)}]
implicationoOII x1 x2 = msum [do {let {x0 = Trueo};
                                  guard (x2 == Trueo);
                                  guard (x1 == Falso);
                                  return x0},
                              do {let {x0 = Trueo};
                                  guard (x2 == Falso);
                                  guard (x1 == Falso);
                                  return x0},
                              do {let {x0 = Trueo};
                                  guard (x2 == Trueo);
                                  guard (x1 == Trueo);
                                  return x0},
                              do {let {x0 = Falso};
                                  guard (x2 == Falso);
                                  guard (x1 == Trueo);
                                  return x0}]
implicationoEvaloEvaloEvaloEvaloIIOO x0 x1 = msum [do {x2 <- __evaloEvaloIO x0;
                                                       x3 <- ___evaloEvaloIO x1;
                                                       return (x2, x3)},
                                                   do {x2 <- __evaloEvaloIO x0;
                                                       x3 <- __evaloEvaloIO x1;
                                                       return (x2, x3)},
                                                   do {x2 <- ___evaloEvaloIO x0;
                                                       x3 <- ___evaloEvaloIO x1;
                                                       return (x2, x3)}]
implicationoEvaloEvaloEvaloEvaloOOII x2 x3 = msum [do {x0 <- __evaloEvaloOI x2;
                                                       x1 <- ___evaloEvaloOI x3;
                                                       return (x0, x1)},
                                                   do {x0 <- __evaloEvaloOI x2;
                                                       x1 <- __evaloEvaloOI x3;
                                                       return (x0, x1)},
                                                   do {x0 <- ___evaloEvaloOI x2;
                                                       x1 <- ___evaloEvaloOI x3;
                                                       return (x0, x1)}]
notoII x0 x1 = msum [do {guard (x1 == Trueo);
                         guard (x0 == Falso);
                         return ()},
                     do {guard (x1 == Falso); guard (x0 == Trueo); return ()}]
notoIO x0 = msum [do {let {x1 = Trueo};
                      guard (x0 == Falso);
                      return x1},
                  do {let {x1 = Falso}; guard (x0 == Trueo); return x1}]
notoOI x1 = msum [do {let {x0 = Falso};
                      guard (x1 == Trueo);
                      return x0},
                  do {let {x0 = Trueo}; guard (x1 == Falso); return x0}]
oroIII x0 x1 x2 = msum [do {guard (x2 == Trueo);
                            guard (x1 == Trueo);
                            guard (x0 == Trueo);
                            return ()},
                        do {guard (x2 == Trueo);
                            guard (x1 == Falso);
                            guard (x0 == Trueo);
                            return ()},
                        do {guard (x2 == Falso);
                            guard (x1 == Trueo);
                            guard (x0 == Trueo);
                            return ()},
                        do {guard (x2 == Falso);
                            guard (x1 == Falso);
                            guard (x0 == Falso);
                            return ()}]
oroIOO x0 = msum [do {let {x2 = Trueo};
                      let {x1 = Trueo};
                      guard (x0 == Trueo);
                      return (x1, x2)},
                  do {let {x2 = Trueo};
                      let {x1 = Falso};
                      guard (x0 == Trueo);
                      return (x1, x2)},
                  do {let {x2 = Falso};
                      let {x1 = Trueo};
                      guard (x0 == Trueo);
                      return (x1, x2)},
                  do {let {x2 = Falso};
                      let {x1 = Falso};
                      guard (x0 == Falso);
                      return (x1, x2)}]
oroOII x1 x2 = msum [do {let {x0 = Trueo};
                         guard (x2 == Trueo);
                         guard (x1 == Trueo);
                         return x0},
                     do {let {x0 = Trueo};
                         guard (x2 == Trueo);
                         guard (x1 == Falso);
                         return x0},
                     do {let {x0 = Trueo};
                         guard (x2 == Falso);
                         guard (x1 == Trueo);
                         return x0},
                     do {let {x0 = Falso};
                         guard (x2 == Falso);
                         guard (x1 == Falso);
                         return x0}]
oroEvaloEvaloEvaloEvaloIIOO x0 x1 = msum [do {x2 <- ___evaloEvaloIO x0;
                                              x3 <- ___evaloEvaloIO x1;
                                              return (x2, x3)},
                                          do {x2 <- __evaloEvaloIO x0;
                                              x3 <- ___evaloEvaloIO x1;
                                              return (x2, x3)},
                                          do {x2 <- ___evaloEvaloIO x0;
                                              x3 <- __evaloEvaloIO x1;
                                              return (x2, x3)}]
oroEvaloEvaloEvaloEvaloOOII x2 x3 = msum [do {x0 <- ___evaloEvaloOI x2;
                                              x1 <- ___evaloEvaloOI x3;
                                              return (x0, x1)},
                                          do {x0 <- __evaloEvaloOI x2;
                                              x1 <- ___evaloEvaloOI x3;
                                              return (x0, x1)},
                                          do {x0 <- ___evaloEvaloOI x2;
                                              x1 <- __evaloEvaloOI x3;
                                              return (x0, x1)}]