module Prop_offline where

import Stream
import Control.Monad
import Term


evalosdsI x0 = Immature $ msum [do {let {x8 = Trueo};
                         x9 <- case x0 of
                               {Lit y9 -> return y9; _ -> mzero};
                         guard (x9 == x8);
                         return ()},
                     do {x1 <- case x0 of
                               {Neg y1 -> return y1; _ -> mzero};
                         _evalosdsI x1;
                         return ()},
                     do {(x2, x3) <- case x0 of
                                     {Disj y2 y3 -> return (y2, y3); _ -> mzero};
                         evalosdsI x2;
                         evalosdsI x3;
                         return ()},
                     do {(x2, x3) <- case x0 of
                                     {Disj y2 y3 -> return (y2, y3); _ -> mzero};
                         _evalosdsI x2;
                         evalosdsI x3;
                         return ()},
                     do {(x2, x3) <- case x0 of
                                     {Disj y2 y3 -> return (y2, y3); _ -> mzero};
                         evalosdsI x2;
                         _evalosdsI x3;
                         return ()},
                     do {(x4, x5) <- case x0 of
                                     {Conj y4 y5 -> return (y4, y5); _ -> mzero};
                         evalosdsI x4;
                         evalosdsI x5;
                         return ()},
                     do {(x6, x7) <- case x0 of
                                     {Impl y6 y7 -> return (y6, y7); _ -> mzero};
                         _evalosdsI x6;
                         evalosdsI x7;
                         return ()},
                     do {(x6, x7) <- case x0 of
                                     {Impl y6 y7 -> return (y6, y7); _ -> mzero};
                         _evalosdsI x6;
                         _evalosdsI x7;
                         return ()},
                     do {(x6, x7) <- case x0 of
                                     {Impl y6 y7 -> return (y6, y7); _ -> mzero};
                         evalosdsI x6;
                         evalosdsI x7;
                         return ()}]
_evalosdsI x0 = Immature $ msum [do {let {x10 = Falso};
                          x11 <- case x0 of
                                 {Lit y11 -> return y11; _ -> mzero};
                          guard (x11 == x10);
                          return ()},
                      do {x1 <- case x0 of
                                {Neg y1 -> return y1; _ -> mzero};
                          evalosdsI x1;
                          return ()},
                      do {(x2, x3) <- case x0 of
                                      {Disj y2 y3 -> return (y2, y3); _ -> mzero};
                          _evalosdsI x2;
                          _evalosdsI x3;
                          return ()},
                      do {(x4, x5) <- case x0 of
                                      {Conj y4 y5 -> return (y4, y5); _ -> mzero};
                          _evalosdsI x4;
                          evalosdsI x5;
                          return ()},
                      do {(x4, x5) <- case x0 of
                                      {Conj y4 y5 -> return (y4, y5); _ -> mzero};
                          evalosdsI x4;
                          _evalosdsI x5;
                          return ()},
                      do {(x4, x5) <- case x0 of
                                      {Conj y4 y5 -> return (y4, y5); _ -> mzero};
                          _evalosdsI x4;
                          _evalosdsI x5;
                          return ()},
                      do {(x6, x7) <- case x0 of
                                      {Impl y6 y7 -> return (y6, y7); _ -> mzero};
                          evalosdsI x6;
                          _evalosdsI x7;
                          return ()}]
evalosdsO = Immature $ msum [do {let {x8 = Trueo};
                      let {x9 = x8};
                      let {x0 = Lit x9};
                      return x0},
                  do {x1 <- _evalosdsO; let {x0 = Neg x1}; return x0},
                  do {x2 <- evalosdsO;
                      x3 <- evalosdsO;
                      let {x0 = Disj x2 x3};
                      return x0},
                  do {x2 <- _evalosdsO;
                      x3 <- evalosdsO;
                      let {x0 = Disj x2 x3};
                      return x0},
                  do {x2 <- evalosdsO;
                      x3 <- _evalosdsO;
                      let {x0 = Disj x2 x3};
                      return x0},
                  do {x4 <- evalosdsO;
                      x5 <- evalosdsO;
                      let {x0 = Conj x4 x5};
                      return x0},
                  do {x6 <- _evalosdsO;
                      x7 <- evalosdsO;
                      let {x0 = Impl x6 x7};
                      return x0},
                  do {x6 <- _evalosdsO;
                      x7 <- _evalosdsO;
                      let {x0 = Impl x6 x7};
                      return x0},
                  do {x6 <- evalosdsO;
                      x7 <- evalosdsO;
                      let {x0 = Impl x6 x7};
                      return x0}]
_evalosdsO = Immature $ msum [do {let {x10 = Falso};
                       let {x11 = x10};
                       let {x0 = Lit x11};
                       return x0},
                   do {x1 <- evalosdsO; let {x0 = Neg x1}; return x0},
                   do {x2 <- _evalosdsO;
                       x3 <- _evalosdsO;
                       let {x0 = Disj x2 x3};
                       return x0},
                   do {x4 <- _evalosdsO;
                       x5 <- evalosdsO;
                       let {x0 = Conj x4 x5};
                       return x0},
                   do {x4 <- evalosdsO;
                       x5 <- _evalosdsO;
                       let {x0 = Conj x4 x5};
                       return x0},
                   do {x4 <- _evalosdsO;
                       x5 <- _evalosdsO;
                       let {x0 = Conj x4 x5};
                       return x0},
                   do {x6 <- evalosdsO;
                       x7 <- _evalosdsO;
                       let {x0 = Impl x6 x7};
                       return x0}]