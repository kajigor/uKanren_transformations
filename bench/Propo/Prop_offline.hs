module Prop_offline where

import Stream
import Control.Monad
import Term


evaloI x0 = Immature $ msum [do {let {x3 = Trueo};
                      x4 <- case x0 of
                            {Lit y4 -> return y4; _ -> mzero};
                      guard (x4 == x3);
                      return ()},
                  do {let {x5 = Zero};
                      x6 <- case x0 of
                            {Var y6 -> return y6; _ -> mzero};
                      guard (x6 == x5);
                      return ()},
                  do {let {x8 = Zero};
                      let {x7 = Succ x8};
                      x9 <- case x0 of
                            {Var y9 -> return y9; _ -> mzero};
                      guard (x9 == x7);
                      return ()},
                  do {let {x12 = Zero};
                      let {x11 = Succ x12};
                      let {x10 = Succ x11};
                      x13 <- case x0 of
                             {Var y13 -> return y13; _ -> mzero};
                      guard (x13 == x10);
                      return ()},
                  do {x1 <- case x0 of
                            {Neg y1 -> return y1; _ -> mzero};
                      _evaloI x1;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                      evaloI x1;
                      evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                      _evaloI x1;
                      evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                      evaloI x1;
                      _evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                      evaloI x1;
                      evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                      _evaloI x1;
                      evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                      _evaloI x1;
                      _evaloI x2;
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                      evaloI x1;
                      evaloI x2;
                      return ()}]
_evaloI x0 = Immature $ msum [do {let {x14 = Falso};
                       x15 <- case x0 of
                              {Lit y15 -> return y15; _ -> mzero};
                       guard (x15 == x14);
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
                       _evaloI x1;
                       evaloI x2;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                       evaloI x1;
                       _evaloI x2;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                       _evaloI x1;
                       _evaloI x2;
                       return ()},
                   do {(x1, x2) <- case x0 of
                                   {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                       evaloI x1;
                       _evaloI x2;
                       return ()}]
evaloO = Immature $ msum [do {let {x3 = Trueo};
                   let {x4 = x3};
                   let {x0 = Lit x4};
                   return x0},
               do {let {x5 = Zero}; let {x6 = x5}; let {x0 = Var x6}; return x0},
               do {let {x8 = Zero};
                   let {x7 = Succ x8};
                   let {x9 = x7};
                   let {x0 = Var x9};
                   return x0},
               do {let {x12 = Zero};
                   let {x11 = Succ x12};
                   let {x10 = Succ x11};
                   let {x13 = x10};
                   let {x0 = Var x13};
                   return x0},
               do {x1 <- _evaloO; let {x0 = Neg x1}; return x0},
               do {x1 <- evaloO; x2 <- evaloO; let {x0 = Disj x1 x2}; return x0},
               do {x1 <- _evaloO; x2 <- evaloO; let {x0 = Disj x1 x2}; return x0},
               do {x1 <- evaloO; x2 <- _evaloO; let {x0 = Disj x1 x2}; return x0},
               do {x1 <- evaloO; x2 <- evaloO; let {x0 = Conj x1 x2}; return x0},
               do {x1 <- _evaloO; x2 <- evaloO; let {x0 = Impl x1 x2}; return x0},
               do {x1 <- _evaloO;
                   x2 <- _evaloO;
                   let {x0 = Impl x1 x2};
                   return x0},
               do {x1 <- evaloO; x2 <- evaloO; let {x0 = Impl x1 x2}; return x0}]
_evaloO = Immature $ msum [do {let {x14 = Falso};
                    let {x15 = x14};
                    let {x0 = Lit x15};
                    return x0},
                do {x1 <- evaloO; let {x0 = Neg x1}; return x0},
                do {x1 <- _evaloO;
                    x2 <- _evaloO;
                    let {x0 = Disj x1 x2};
                    return x0},
                do {x1 <- _evaloO; x2 <- evaloO; let {x0 = Conj x1 x2}; return x0},
                do {x1 <- evaloO; x2 <- _evaloO; let {x0 = Conj x1 x2}; return x0},
                do {x1 <- _evaloO;
                    x2 <- _evaloO;
                    let {x0 = Conj x1 x2};
                    return x0},
                do {x1 <- evaloO; x2 <- _evaloO; let {x0 = Impl x1 x2}; return x0}]