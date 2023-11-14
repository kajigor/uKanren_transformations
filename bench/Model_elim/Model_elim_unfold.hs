module Model_elim_unfold where

import Stream
import Control.Monad
import Term

solveIII x0 x1 x2 = msum [do {let {x7 = Nil};
                              _contrapositiveProveallIIII x0 x1 x2 x7;
                              return ()}]
_contrapositiveProveallIIII x0 x1 x2 x3 = msum [do {_input_clauseProveallIIII x0 x1 x2 x3;
                                                    return ()}]
_input_clauseProveallIIII x0 x1 x2 x3 = msum [do {guard (x1 == x2);
                                                  guard (x0 == Nil);
                                                  return ()},
                                              do {(x4, x5) <- case x2 of
                                                              {Cons y4 y5 -> return (y4, y5);
                                                               _ -> mzero};
                                                  (x8, x6) <- case x0 of
                                                              {Cons y8 y6 -> return (y8, y6);
                                                               _ -> mzero};
                                                  guard (x8 == x4);
                                                  _proveIIIII x3 x4 x6 x1 x5;
                                                  return ()}]
_proveIIIII x0 x1 x2 x3 x4 = msum [do {memberIIII x2 x3 x4 x0;
                                       return ()},
                                   do {let {x13 = x2};
                                       let {x12 = Cons x1 x13};
                                       let {x14 = x3};
                                       let {x16 = x1};
                                       let {x17 = x4};
                                       let {x15 = Cons x16 x17};
                                       let {x11 = App x12 x14 x15};
                                       let {x10 = Pos x11};
                                       let {x9 = Cons x10 x0};
                                       _contrapositiveProveallIIII x2 x3 x4 x9;
                                       return ()}]
memberIIII x0 x1 x2 x3 = msum [do {let {x19 = App x0 x1 x2};
                                   let {x18 = Neg x19};
                                   (x20, x4) <- case x3 of
                                                {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                                   guard (x20 == x18);
                                   return ()},
                               do {(x5, x6) <- case x3 of
                                               {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                   memberIIII x0 x1 x2 x6;
                                   return ()}]
solveIIO x0 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {let {x7 = Nil};
                                                                                                                   x2 <- _contrapositiveProveallIIOI x0 x1 x7 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                   return x2}]
_contrapositiveProveallIIOI x0 x1 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {x2 <- _input_clauseProveallIIOI x0 x1 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                                         return x2}]
_input_clauseProveallIIOI x0 x1 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                       let {x2 = x1};
                                                                                                                                       return x2},
                                                                                                                                   do {(x8,
                                                                                                                                        x6) <- case x0 of
                                                                                                                                               {Cons y8
                                                                                                                                                     y6 -> return (y8,
                                                                                                                                                                   y6);
                                                                                                                                                _ -> mzero};
                                                                                                                                       let {x4 = x8};
                                                                                                                                       x5 <- _proveIIIIO x3 x4 x6 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                                       let {x2 = Cons x4 x5};
                                                                                                                                       return x2}]
_proveIIIIO x0 x1 x2 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {x4 <- memberIIOI x2 x3 x0;
                                                                                                                            return x4},
                                                                                                                        do {let {x13 = x2};
                                                                                                                            let {x12 = Cons x1 x13};
                                                                                                                            let {x14 = x3};
                                                                                                                            let {x16 = x1};
                                                                                                                            (x4,
                                                                                                                             x9) <- _contrapositiveProveallIIOO x2 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                            x10 <- case x9 of
                                                                                                                                   {Cons y10
                                                                                                                                         y0 -> do {guard (x0 == y0);
                                                                                                                                                   return y10};
                                                                                                                                    _ -> mzero};
                                                                                                                            x11 <- case x10 of
                                                                                                                                   {Pos y11 -> return y11;
                                                                                                                                    _ -> mzero};
                                                                                                                            x15 <- case x11 of
                                                                                                                                   {App y12
                                                                                                                                        y14
                                                                                                                                        y15 -> do {guard (x12 == y12);
                                                                                                                                                   guard (x14 == y14);
                                                                                                                                                   return y15};
                                                                                                                                    _ -> mzero};
                                                                                                                            x17 <- case x15 of
                                                                                                                                   {Cons y16
                                                                                                                                         y17 -> do {guard (x16 == y16);
                                                                                                                                                    return y17};
                                                                                                                                    _ -> mzero};
                                                                                                                            guard (x17 == x4);
                                                                                                                            return x4}]
_contrapositiveProveallIIOO x0 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {(x2,
                                                                                                                                       x3) <- _input_clauseProveallIIOO x0 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                                      return (x2,
                                                                                                                                              x3)}]
_input_clauseProveallIIOO x0 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                    let {x2 = x1};
                                                                                                                                    x3 <- gen__input_clauseProveallIIOO_x3;
                                                                                                                                    return (x2,
                                                                                                                                            x3)},
                                                                                                                                do {(x8,
                                                                                                                                     x6) <- case x0 of
                                                                                                                                            {Cons y8
                                                                                                                                                  y6 -> return (y8,
                                                                                                                                                                y6);
                                                                                                                                             _ -> mzero};
                                                                                                                                    let {x4 = x8};
                                                                                                                                    (x3,
                                                                                                                                     x5) <- _proveOIIIO x4 x6 x1 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                                    let {x2 = Cons x4 x5};
                                                                                                                                    return (x2,
                                                                                                                                            x3)}]
_proveOIIIO x1 x2 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {(x4,
                                                                                                                          x0) <- memberIIOO x2 x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                         return (x0,
                                                                                                                                 x4)},
                                                                                                                     do {let {x13 = x2};
                                                                                                                         let {x12 = Cons x1 x13};
                                                                                                                         let {x14 = x3};
                                                                                                                         let {x16 = x1};
                                                                                                                         (x4,
                                                                                                                          x9) <- _contrapositiveProveallIIOO x2 x3 gen__input_clauseProveallIIOO_x3 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                                                         (x10,
                                                                                                                          x0) <- case x9 of
                                                                                                                                 {Cons y10
                                                                                                                                       y0 -> return (y10,
                                                                                                                                                     y0);
                                                                                                                                  _ -> mzero};
                                                                                                                         x11 <- case x10 of
                                                                                                                                {Pos y11 -> return y11;
                                                                                                                                 _ -> mzero};
                                                                                                                         x15 <- case x11 of
                                                                                                                                {App y12
                                                                                                                                     y14
                                                                                                                                     y15 -> do {guard (x12 == y12);
                                                                                                                                                guard (x14 == y14);
                                                                                                                                                return y15};
                                                                                                                                 _ -> mzero};
                                                                                                                         x17 <- case x15 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> do {guard (x16 == y16);
                                                                                                                                                 return y17};
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x17 == x4);
                                                                                                                         return (x0,
                                                                                                                                 x4)}]
memberIIOI x0 x1 x3 = msum [do {(x20, x4) <- case x3 of
                                             {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                                let {x18 = x20};
                                x19 <- case x18 of
                                       {Neg y19 -> return y19; _ -> mzero};
                                x2 <- case x19 of
                                      {App y0 y1 y2 -> do {guard (x0 == y0);
                                                           guard (x1 == y1);
                                                           return y2};
                                       _ -> mzero};
                                return x2},
                            do {(x5, x6) <- case x3 of
                                            {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                x2 <- memberIIOI x0 x1 x6;
                                return x2}]
memberIIOO x0 x1 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5 = msum [do {(x20,
                                                                                     x18) <- do {x18 <- gen_memberIIOO_x18;
                                                                                                 return (x18,
                                                                                                         x18)};
                                                                                    x19 <- case x18 of
                                                                                           {Neg y19 -> return y19;
                                                                                            _ -> mzero};
                                                                                    x2 <- case x19 of
                                                                                          {App y0
                                                                                               y1
                                                                                               y2 -> do {guard (x0 == y0);
                                                                                                         guard (x1 == y1);
                                                                                                         return y2};
                                                                                           _ -> mzero};
                                                                                    (x3,
                                                                                     x4) <- do {x4 <- gen_memberIIOO_x4;
                                                                                                let {x3 = Cons x20 x4};
                                                                                                return (x3,
                                                                                                        x4)};
                                                                                    return (x2,
                                                                                            x3)},
                                                                                do {(x2,
                                                                                     x6) <- memberIIOO x0 x1 gen_memberIIOO_x18 gen_memberIIOO_x4 gen_memberIIOO_x5;
                                                                                    (x3,
                                                                                     x5) <- do {x5 <- gen_memberIIOO_x5;
                                                                                                let {x3 = Cons x5 x6};
                                                                                                return (x3,
                                                                                                        x5)};
                                                                                    return (x2,
                                                                                            x3)}]
solveIOI x0 x2 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {let {x7 = Nil};
                                                                                                                   x1 <- _contrapositiveProveallIOII x0 x2 x7 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                   return x1}]
_contrapositiveProveallIOII x0 x2 x3 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {x1 <- _input_clauseProveallIOII x0 x2 x3 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                                         return x1}]
_input_clauseProveallIOII x0 x2 x3 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                       let {x1 = x2};
                                                                                                                                       return x1},
                                                                                                                                   do {(x4,
                                                                                                                                        x5) <- case x2 of
                                                                                                                                               {Cons y4
                                                                                                                                                     y5 -> return (y4,
                                                                                                                                                                   y5);
                                                                                                                                                _ -> mzero};
                                                                                                                                       (x8,
                                                                                                                                        x6) <- case x0 of
                                                                                                                                               {Cons y8
                                                                                                                                                     y6 -> return (y8,
                                                                                                                                                                   y6);
                                                                                                                                                _ -> mzero};
                                                                                                                                       guard (x8 == x4);
                                                                                                                                       x1 <- _proveIIIOI x3 x4 x6 x5 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                                       return x1}]
_proveIIIOI x0 x1 x2 x4 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {x3 <- memberIOII x2 x4 x0;
                                                                                                                            return x3},
                                                                                                                        do {let {x13 = x2};
                                                                                                                            let {x12 = Cons x1 x13};
                                                                                                                            let {x16 = x1};
                                                                                                                            let {x17 = x4};
                                                                                                                            let {x15 = Cons x16 x17};
                                                                                                                            (x3,
                                                                                                                             x9) <- _contrapositiveProveallIOIO x2 x4 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                            x10 <- case x9 of
                                                                                                                                   {Cons y10
                                                                                                                                         y0 -> do {guard (x0 == y0);
                                                                                                                                                   return y10};
                                                                                                                                    _ -> mzero};
                                                                                                                            x11 <- case x10 of
                                                                                                                                   {Pos y11 -> return y11;
                                                                                                                                    _ -> mzero};
                                                                                                                            x14 <- case x11 of
                                                                                                                                   {App y12
                                                                                                                                        y14
                                                                                                                                        y15 -> do {guard (x12 == y12);
                                                                                                                                                   guard (x15 == y15);
                                                                                                                                                   return y14};
                                                                                                                                    _ -> mzero};
                                                                                                                            guard (x14 == x3);
                                                                                                                            return x3}]
_contrapositiveProveallIOIO x0 x2 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {(x1,
                                                                                                                                       x3) <- _input_clauseProveallIOIO x0 x2 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                                      return (x1,
                                                                                                                                              x3)}]
_input_clauseProveallIOIO x0 x2 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                    let {x1 = x2};
                                                                                                                                    x3 <- gen__input_clauseProveallIOIO_x3;
                                                                                                                                    return (x1,
                                                                                                                                            x3)},
                                                                                                                                do {(x4,
                                                                                                                                     x5) <- case x2 of
                                                                                                                                            {Cons y4
                                                                                                                                                  y5 -> return (y4,
                                                                                                                                                                y5);
                                                                                                                                             _ -> mzero};
                                                                                                                                    (x8,
                                                                                                                                     x6) <- case x0 of
                                                                                                                                            {Cons y8
                                                                                                                                                  y6 -> return (y8,
                                                                                                                                                                y6);
                                                                                                                                             _ -> mzero};
                                                                                                                                    guard (x8 == x4);
                                                                                                                                    (x3,
                                                                                                                                     x1) <- _proveOIIOI x4 x6 x5 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                                    return (x1,
                                                                                                                                            x3)}]
_proveOIIOI x1 x2 x4 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {(x3,
                                                                                                                          x0) <- memberIOIO x2 x4 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                         return (x0,
                                                                                                                                 x3)},
                                                                                                                     do {let {x13 = x2};
                                                                                                                         let {x12 = Cons x1 x13};
                                                                                                                         let {x16 = x1};
                                                                                                                         let {x17 = x4};
                                                                                                                         let {x15 = Cons x16 x17};
                                                                                                                         (x3,
                                                                                                                          x9) <- _contrapositiveProveallIOIO x2 x4 gen__input_clauseProveallIOIO_x3 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                                                         (x10,
                                                                                                                          x0) <- case x9 of
                                                                                                                                 {Cons y10
                                                                                                                                       y0 -> return (y10,
                                                                                                                                                     y0);
                                                                                                                                  _ -> mzero};
                                                                                                                         x11 <- case x10 of
                                                                                                                                {Pos y11 -> return y11;
                                                                                                                                 _ -> mzero};
                                                                                                                         x14 <- case x11 of
                                                                                                                                {App y12
                                                                                                                                     y14
                                                                                                                                     y15 -> do {guard (x12 == y12);
                                                                                                                                                guard (x15 == y15);
                                                                                                                                                return y14};
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x14 == x3);
                                                                                                                         return (x0,
                                                                                                                                 x3)}]
memberIOII x0 x2 x3 = msum [do {(x20, x4) <- case x3 of
                                             {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                                let {x18 = x20};
                                x19 <- case x18 of
                                       {Neg y19 -> return y19; _ -> mzero};
                                x1 <- case x19 of
                                      {App y0 y1 y2 -> do {guard (x0 == y0);
                                                           guard (x2 == y2);
                                                           return y1};
                                       _ -> mzero};
                                return x1},
                            do {(x5, x6) <- case x3 of
                                            {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                x1 <- memberIOII x0 x2 x6;
                                return x1}]
memberIOIO x0 x2 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5 = msum [do {(x20,
                                                                                     x18) <- do {x18 <- gen_memberIOIO_x18;
                                                                                                 return (x18,
                                                                                                         x18)};
                                                                                    x19 <- case x18 of
                                                                                           {Neg y19 -> return y19;
                                                                                            _ -> mzero};
                                                                                    x1 <- case x19 of
                                                                                          {App y0
                                                                                               y1
                                                                                               y2 -> do {guard (x0 == y0);
                                                                                                         guard (x2 == y2);
                                                                                                         return y1};
                                                                                           _ -> mzero};
                                                                                    (x3,
                                                                                     x4) <- do {x4 <- gen_memberIOIO_x4;
                                                                                                let {x3 = Cons x20 x4};
                                                                                                return (x3,
                                                                                                        x4)};
                                                                                    return (x1,
                                                                                            x3)},
                                                                                do {(x1,
                                                                                     x6) <- memberIOIO x0 x2 gen_memberIOIO_x18 gen_memberIOIO_x4 gen_memberIOIO_x5;
                                                                                    (x3,
                                                                                     x5) <- do {x5 <- gen_memberIOIO_x5;
                                                                                                let {x3 = Cons x5 x6};
                                                                                                return (x3,
                                                                                                        x5)};
                                                                                    return (x1,
                                                                                            x3)}]
solveIOO x0 gen__input_clauseProveallIOOI_x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {let {x7 = Nil};
                                                                                                                                                                                  (x1,
                                                                                                                                                                                   x2) <- _contrapositiveProveallIOOI x0 x7 gen__input_clauseProveallIOOI_x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                  return (x1,
                                                                                                                                                                                          x2)}]
_contrapositiveProveallIOOI x0 x3 gen__input_clauseProveallIOOI_x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x1,
                                                                                                                                                                                                         x2) <- _input_clauseProveallIOOI x0 x3 gen__input_clauseProveallIOOI_x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                x2)}]
_input_clauseProveallIOOI x0 x3 gen__input_clauseProveallIOOI_x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                                                                                      (x1,
                                                                                                                                                                                                       x2) <- do {x2 <- gen__input_clauseProveallIOOI_x2;
                                                                                                                                                                                                                  return (x2,
                                                                                                                                                                                                                          x2)};
                                                                                                                                                                                                      return (x1,
                                                                                                                                                                                                              x2)},
                                                                                                                                                                                                  do {(x8,
                                                                                                                                                                                                       x6) <- case x0 of
                                                                                                                                                                                                              {Cons y8
                                                                                                                                                                                                                    y6 -> return (y8,
                                                                                                                                                                                                                                  y6);
                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                      let {x4 = x8};
                                                                                                                                                                                                      (x1,
                                                                                                                                                                                                       x5) <- _proveIIIOO x3 x4 x6 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                                      let {x2 = Cons x4 x5};
                                                                                                                                                                                                      return (x1,
                                                                                                                                                                                                              x2)}]
_proveIIIOO x0 x1 x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x3,
                                                                                                                                                           x4) <- memberIOOI x2 x0;
                                                                                                                                                          return (x3,
                                                                                                                                                                  x4)},
                                                                                                                                                      do {let {x13 = x2};
                                                                                                                                                          let {x12 = Cons x1 x13};
                                                                                                                                                          let {x16 = x1};
                                                                                                                                                          (x3,
                                                                                                                                                           x4,
                                                                                                                                                           x9) <- _contrapositiveProveallIOOO x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                          x10 <- case x9 of
                                                                                                                                                                 {Cons y10
                                                                                                                                                                       y0 -> do {guard (x0 == y0);
                                                                                                                                                                                 return y10};
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                          x11 <- case x10 of
                                                                                                                                                                 {Pos y11 -> return y11;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                          (x14,
                                                                                                                                                           x15) <- case x11 of
                                                                                                                                                                   {App y12
                                                                                                                                                                        y14
                                                                                                                                                                        y15 -> do {guard (x12 == y12);
                                                                                                                                                                                   return (y14,
                                                                                                                                                                                           y15)};
                                                                                                                                                                    _ -> mzero};
                                                                                                                                                          guard (x14 == x3);
                                                                                                                                                          x17 <- case x15 of
                                                                                                                                                                 {Cons y16
                                                                                                                                                                       y17 -> do {guard (x16 == y16);
                                                                                                                                                                                  return y17};
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                          guard (x17 == x4);
                                                                                                                                                          return (x3,
                                                                                                                                                                  x4)}]
_contrapositiveProveallIOOO x0 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x1,
                                                                                                                                                                     x2,
                                                                                                                                                                     x3) <- _input_clauseProveallIOOO x0 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                    return (x1,
                                                                                                                                                                            x2,
                                                                                                                                                                            x3)}]
_input_clauseProveallIOOO x0 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {guard (x0 == Nil);
                                                                                                                                                                  (x1,
                                                                                                                                                                   x2) <- do {x2 <- gen__input_clauseProveallIOOO_x2;
                                                                                                                                                                              return (x2,
                                                                                                                                                                                      x2)};
                                                                                                                                                                  x3 <- gen__input_clauseProveallIOOO_x3;
                                                                                                                                                                  return (x1,
                                                                                                                                                                          x2,
                                                                                                                                                                          x3)},
                                                                                                                                                              do {(x8,
                                                                                                                                                                   x6) <- case x0 of
                                                                                                                                                                          {Cons y8
                                                                                                                                                                                y6 -> return (y8,
                                                                                                                                                                                              y6);
                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                  let {x4 = x8};
                                                                                                                                                                  (x3,
                                                                                                                                                                   x1,
                                                                                                                                                                   x5) <- _proveOIIOO x4 x6 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                  let {x2 = Cons x4 x5};
                                                                                                                                                                  return (x1,
                                                                                                                                                                          x2,
                                                                                                                                                                          x3)}]
_proveOIIOO x1 x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x3,
                                                                                                                                                        x4,
                                                                                                                                                        x0) <- memberIOOO x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                       return (x0,
                                                                                                                                                               x3,
                                                                                                                                                               x4)},
                                                                                                                                                   do {let {x13 = x2};
                                                                                                                                                       let {x12 = Cons x1 x13};
                                                                                                                                                       let {x16 = x1};
                                                                                                                                                       (x3,
                                                                                                                                                        x4,
                                                                                                                                                        x9) <- _contrapositiveProveallIOOO x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                       (x10,
                                                                                                                                                        x0) <- case x9 of
                                                                                                                                                               {Cons y10
                                                                                                                                                                     y0 -> return (y10,
                                                                                                                                                                                   y0);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       x11 <- case x10 of
                                                                                                                                                              {Pos y11 -> return y11;
                                                                                                                                                               _ -> mzero};
                                                                                                                                                       (x14,
                                                                                                                                                        x15) <- case x11 of
                                                                                                                                                                {App y12
                                                                                                                                                                     y14
                                                                                                                                                                     y15 -> do {guard (x12 == y12);
                                                                                                                                                                                return (y14,
                                                                                                                                                                                        y15)};
                                                                                                                                                                 _ -> mzero};
                                                                                                                                                       guard (x14 == x3);
                                                                                                                                                       x17 <- case x15 of
                                                                                                                                                              {Cons y16
                                                                                                                                                                    y17 -> do {guard (x16 == y16);
                                                                                                                                                                               return y17};
                                                                                                                                                               _ -> mzero};
                                                                                                                                                       guard (x17 == x4);
                                                                                                                                                       return (x0,
                                                                                                                                                               x3,
                                                                                                                                                               x4)}]
memberIOOI x0 x3 = msum [do {(x20, x4) <- case x3 of
                                          {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                             let {x18 = x20};
                             x19 <- case x18 of
                                    {Neg y19 -> return y19; _ -> mzero};
                             (x1, x2) <- case x19 of
                                         {App y0 y1 y2 -> do {guard (x0 == y0); return (y1, y2)};
                                          _ -> mzero};
                             return (x1, x2)},
                         do {(x5, x6) <- case x3 of
                                         {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                             (x1, x2) <- memberIOOI x0 x6;
                             return (x1, x2)}]
memberIOOO x0 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x20,
                                                                                  x18) <- do {x18 <- gen_memberIOOO_x18;
                                                                                              return (x18,
                                                                                                      x18)};
                                                                                 x19 <- case x18 of
                                                                                        {Neg y19 -> return y19;
                                                                                         _ -> mzero};
                                                                                 (x1,
                                                                                  x2) <- case x19 of
                                                                                         {App y0
                                                                                              y1
                                                                                              y2 -> do {guard (x0 == y0);
                                                                                                        return (y1,
                                                                                                                y2)};
                                                                                          _ -> mzero};
                                                                                 (x3,
                                                                                  x4) <- do {x4 <- gen_memberIOOO_x4;
                                                                                             let {x3 = Cons x20 x4};
                                                                                             return (x3,
                                                                                                     x4)};
                                                                                 return (x1,
                                                                                         x2,
                                                                                         x3)},
                                                                             do {(x1,
                                                                                  x2,
                                                                                  x6) <- memberIOOO x0 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                 (x3,
                                                                                  x5) <- do {x5 <- gen_memberIOOO_x5;
                                                                                             let {x3 = Cons x5 x6};
                                                                                             return (x3,
                                                                                                     x5)};
                                                                                 return (x1,
                                                                                         x2,
                                                                                         x3)}]
solveOII x1 x2 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {let {x7 = Nil};
                                                                                                                   x0 <- _contrapositiveProveallOIII x1 x2 x7 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                   return x0}]
_contrapositiveProveallOIII x1 x2 x3 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {x0 <- _input_clauseProveallOIII x1 x2 x3 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                                         return x0}]
_input_clauseProveallOIII x1 x2 x3 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {guard (x1 == x2);
                                                                                                                                       let {x0 = Nil};
                                                                                                                                       return x0},
                                                                                                                                   do {(x4,
                                                                                                                                        x5) <- case x2 of
                                                                                                                                               {Cons y4
                                                                                                                                                     y5 -> return (y4,
                                                                                                                                                                   y5);
                                                                                                                                                _ -> mzero};
                                                                                                                                       let {x8 = x4};
                                                                                                                                       x6 <- _proveIIOII x3 x4 x1 x5 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                                       let {x0 = Cons x8 x6};
                                                                                                                                       return x0}]
_proveIIOII x0 x1 x3 x4 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {x2 <- memberOIII x3 x4 x0;
                                                                                                                            return x2},
                                                                                                                        do {let {x14 = x3};
                                                                                                                            let {x16 = x1};
                                                                                                                            let {x17 = x4};
                                                                                                                            let {x15 = Cons x16 x17};
                                                                                                                            (x2,
                                                                                                                             x9) <- _contrapositiveProveallOIIO x3 x4 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                            x10 <- case x9 of
                                                                                                                                   {Cons y10
                                                                                                                                         y0 -> do {guard (x0 == y0);
                                                                                                                                                   return y10};
                                                                                                                                    _ -> mzero};
                                                                                                                            x11 <- case x10 of
                                                                                                                                   {Pos y11 -> return y11;
                                                                                                                                    _ -> mzero};
                                                                                                                            x12 <- case x11 of
                                                                                                                                   {App y12
                                                                                                                                        y14
                                                                                                                                        y15 -> do {guard (x14 == y14);
                                                                                                                                                   guard (x15 == y15);
                                                                                                                                                   return y12};
                                                                                                                                    _ -> mzero};
                                                                                                                            x13 <- case x12 of
                                                                                                                                   {Cons y1
                                                                                                                                         y13 -> do {guard (x1 == y1);
                                                                                                                                                    return y13};
                                                                                                                                    _ -> mzero};
                                                                                                                            guard (x13 == x2);
                                                                                                                            return x2}]
_contrapositiveProveallOIIO x1 x2 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {(x0,
                                                                                                                                       x3) <- _input_clauseProveallOIIO x1 x2 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                                      return (x0,
                                                                                                                                              x3)}]
_input_clauseProveallOIIO x1 x2 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {guard (x1 == x2);
                                                                                                                                    let {x0 = Nil};
                                                                                                                                    x3 <- gen__input_clauseProveallOIIO_x3;
                                                                                                                                    return (x0,
                                                                                                                                            x3)},
                                                                                                                                do {(x4,
                                                                                                                                     x5) <- case x2 of
                                                                                                                                            {Cons y4
                                                                                                                                                  y5 -> return (y4,
                                                                                                                                                                y5);
                                                                                                                                             _ -> mzero};
                                                                                                                                    let {x8 = x4};
                                                                                                                                    (x3,
                                                                                                                                     x6) <- _proveOIOII x4 x1 x5 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                                    let {x0 = Cons x8 x6};
                                                                                                                                    return (x0,
                                                                                                                                            x3)}]
_proveOIOII x1 x3 x4 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {(x2,
                                                                                                                          x0) <- memberOIIO x3 x4 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                         return (x0,
                                                                                                                                 x2)},
                                                                                                                     do {let {x14 = x3};
                                                                                                                         let {x16 = x1};
                                                                                                                         let {x17 = x4};
                                                                                                                         let {x15 = Cons x16 x17};
                                                                                                                         (x2,
                                                                                                                          x9) <- _contrapositiveProveallOIIO x3 x4 gen__input_clauseProveallOIIO_x3 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                                                         (x10,
                                                                                                                          x0) <- case x9 of
                                                                                                                                 {Cons y10
                                                                                                                                       y0 -> return (y10,
                                                                                                                                                     y0);
                                                                                                                                  _ -> mzero};
                                                                                                                         x11 <- case x10 of
                                                                                                                                {Pos y11 -> return y11;
                                                                                                                                 _ -> mzero};
                                                                                                                         x12 <- case x11 of
                                                                                                                                {App y12
                                                                                                                                     y14
                                                                                                                                     y15 -> do {guard (x14 == y14);
                                                                                                                                                guard (x15 == y15);
                                                                                                                                                return y12};
                                                                                                                                 _ -> mzero};
                                                                                                                         x13 <- case x12 of
                                                                                                                                {Cons y1
                                                                                                                                      y13 -> do {guard (x1 == y1);
                                                                                                                                                 return y13};
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x13 == x2);
                                                                                                                         return (x0,
                                                                                                                                 x2)}]
memberOIII x1 x2 x3 = msum [do {(x20, x4) <- case x3 of
                                             {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                                let {x18 = x20};
                                x19 <- case x18 of
                                       {Neg y19 -> return y19; _ -> mzero};
                                x0 <- case x19 of
                                      {App y0 y1 y2 -> do {guard (x1 == y1);
                                                           guard (x2 == y2);
                                                           return y0};
                                       _ -> mzero};
                                return x0},
                            do {(x5, x6) <- case x3 of
                                            {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                x0 <- memberOIII x1 x2 x6;
                                return x0}]
memberOIIO x1 x2 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5 = msum [do {(x20,
                                                                                     x18) <- do {x18 <- gen_memberOIIO_x18;
                                                                                                 return (x18,
                                                                                                         x18)};
                                                                                    x19 <- case x18 of
                                                                                           {Neg y19 -> return y19;
                                                                                            _ -> mzero};
                                                                                    x0 <- case x19 of
                                                                                          {App y0
                                                                                               y1
                                                                                               y2 -> do {guard (x1 == y1);
                                                                                                         guard (x2 == y2);
                                                                                                         return y0};
                                                                                           _ -> mzero};
                                                                                    (x3,
                                                                                     x4) <- do {x4 <- gen_memberOIIO_x4;
                                                                                                let {x3 = Cons x20 x4};
                                                                                                return (x3,
                                                                                                        x4)};
                                                                                    return (x0,
                                                                                            x3)},
                                                                                do {(x0,
                                                                                     x6) <- memberOIIO x1 x2 gen_memberOIIO_x18 gen_memberOIIO_x4 gen_memberOIIO_x5;
                                                                                    (x3,
                                                                                     x5) <- do {x5 <- gen_memberOIIO_x5;
                                                                                                let {x3 = Cons x5 x6};
                                                                                                return (x3,
                                                                                                        x5)};
                                                                                    return (x0,
                                                                                            x3)}]
solveOIO x1 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {let {x7 = Nil};
                                                                                                                                                      (x0,
                                                                                                                                                       x2) <- _contrapositiveProveallOIOI x1 x7 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                      return (x0,
                                                                                                                                                              x2)}]
_contrapositiveProveallOIOI x1 x3 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {(x0,
                                                                                                                                                                             x2) <- _input_clauseProveallOIOI x1 x3 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                                            return (x0,
                                                                                                                                                                                    x2)}]
_input_clauseProveallOIOI x1 x3 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {let {x0 = Nil};
                                                                                                                                                                          let {x2 = x1};
                                                                                                                                                                          return (x0,
                                                                                                                                                                                  x2)},
                                                                                                                                                                      do {(x4,
                                                                                                                                                                           x6,
                                                                                                                                                                           x5) <- _proveIOOIO x3 x1 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                                          let {x2 = Cons x4 x5};
                                                                                                                                                                          let {x8 = x4};
                                                                                                                                                                          let {x0 = Cons x8 x6};
                                                                                                                                                                          return (x0,
                                                                                                                                                                                  x2)}]
_proveIOOIO x0 x3 gen__input_clauseProveallOIOO_x3 gen__proveIOOIO_x1 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {(x2,
                                                                                                                                                             x4) <- memberOIOI x3 x0;
                                                                                                                                                            x1 <- gen__proveIOOIO_x1;
                                                                                                                                                            return (x1,
                                                                                                                                                                    x2,
                                                                                                                                                                    x4)},
                                                                                                                                                        do {let {x14 = x3};
                                                                                                                                                            (x2,
                                                                                                                                                             x4,
                                                                                                                                                             x9) <- _contrapositiveProveallOIOO x3 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                            x10 <- case x9 of
                                                                                                                                                                   {Cons y10
                                                                                                                                                                         y0 -> do {guard (x0 == y0);
                                                                                                                                                                                   return y10};
                                                                                                                                                                    _ -> mzero};
                                                                                                                                                            x11 <- case x10 of
                                                                                                                                                                   {Pos y11 -> return y11;
                                                                                                                                                                    _ -> mzero};
                                                                                                                                                            (x12,
                                                                                                                                                             x15) <- case x11 of
                                                                                                                                                                     {App y12
                                                                                                                                                                          y14
                                                                                                                                                                          y15 -> do {guard (x14 == y14);
                                                                                                                                                                                     return (y12,
                                                                                                                                                                                             y15)};
                                                                                                                                                                      _ -> mzero};
                                                                                                                                                            (x1,
                                                                                                                                                             x13) <- case x12 of
                                                                                                                                                                     {Cons y1
                                                                                                                                                                           y13 -> return (y1,
                                                                                                                                                                                          y13);
                                                                                                                                                                      _ -> mzero};
                                                                                                                                                            guard (x13 == x2);
                                                                                                                                                            (x16,
                                                                                                                                                             x17) <- case x15 of
                                                                                                                                                                     {Cons y16
                                                                                                                                                                           y17 -> return (y16,
                                                                                                                                                                                          y17);
                                                                                                                                                                      _ -> mzero};
                                                                                                                                                            guard (x16 == x1);
                                                                                                                                                            guard (x17 == x4);
                                                                                                                                                            return (x1,
                                                                                                                                                                    x2,
                                                                                                                                                                    x4)}]
_contrapositiveProveallOIOO x1 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {(x0,
                                                                                                                                                       x2,
                                                                                                                                                       x3) <- _input_clauseProveallOIOO x1 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                      return (x0,
                                                                                                                                                              x2,
                                                                                                                                                              x3)}]
_input_clauseProveallOIOO x1 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {let {x0 = Nil};
                                                                                                                                                    let {x2 = x1};
                                                                                                                                                    x3 <- gen__input_clauseProveallOIOO_x3;
                                                                                                                                                    return (x0,
                                                                                                                                                            x2,
                                                                                                                                                            x3)},
                                                                                                                                                do {(x3,
                                                                                                                                                     x4,
                                                                                                                                                     x6,
                                                                                                                                                     x5) <- _proveOOOIO x1 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                                    let {x2 = Cons x4 x5};
                                                                                                                                                    let {x8 = x4};
                                                                                                                                                    let {x0 = Cons x8 x6};
                                                                                                                                                    return (x0,
                                                                                                                                                            x2,
                                                                                                                                                            x3)}]
_proveOOOIO x3 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {(x2,
                                                                                                                                       x4,
                                                                                                                                       x0) <- memberOIOO x3 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                      x1 <- gen__proveOOOIO_x1;
                                                                                                                                      return (x0,
                                                                                                                                              x1,
                                                                                                                                              x2,
                                                                                                                                              x4)},
                                                                                                                                  do {let {x14 = x3};
                                                                                                                                      (x2,
                                                                                                                                       x4,
                                                                                                                                       x9) <- _contrapositiveProveallOIOO x3 gen__input_clauseProveallOIOO_x3 gen__proveOOOIO_x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                                                                      (x10,
                                                                                                                                       x0) <- case x9 of
                                                                                                                                              {Cons y10
                                                                                                                                                    y0 -> return (y10,
                                                                                                                                                                  y0);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x11 <- case x10 of
                                                                                                                                             {Pos y11 -> return y11;
                                                                                                                                              _ -> mzero};
                                                                                                                                      (x12,
                                                                                                                                       x15) <- case x11 of
                                                                                                                                               {App y12
                                                                                                                                                    y14
                                                                                                                                                    y15 -> do {guard (x14 == y14);
                                                                                                                                                               return (y12,
                                                                                                                                                                       y15)};
                                                                                                                                                _ -> mzero};
                                                                                                                                      (x1,
                                                                                                                                       x13) <- case x12 of
                                                                                                                                               {Cons y1
                                                                                                                                                     y13 -> return (y1,
                                                                                                                                                                    y13);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x13 == x2);
                                                                                                                                      (x16,
                                                                                                                                       x17) <- case x15 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x16 == x1);
                                                                                                                                      guard (x17 == x4);
                                                                                                                                      return (x0,
                                                                                                                                              x1,
                                                                                                                                              x2,
                                                                                                                                              x4)}]
memberOIOI x1 x3 = msum [do {(x20, x4) <- case x3 of
                                          {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                             let {x18 = x20};
                             x19 <- case x18 of
                                    {Neg y19 -> return y19; _ -> mzero};
                             (x0, x2) <- case x19 of
                                         {App y0 y1 y2 -> do {guard (x1 == y1); return (y0, y2)};
                                          _ -> mzero};
                             return (x0, x2)},
                         do {(x5, x6) <- case x3 of
                                         {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                             (x0, x2) <- memberOIOI x1 x6;
                             return (x0, x2)}]
memberOIOO x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5 = msum [do {(x20,
                                                                                  x18) <- do {x18 <- gen_memberOIOO_x18;
                                                                                              return (x18,
                                                                                                      x18)};
                                                                                 x19 <- case x18 of
                                                                                        {Neg y19 -> return y19;
                                                                                         _ -> mzero};
                                                                                 (x0,
                                                                                  x2) <- case x19 of
                                                                                         {App y0
                                                                                              y1
                                                                                              y2 -> do {guard (x1 == y1);
                                                                                                        return (y0,
                                                                                                                y2)};
                                                                                          _ -> mzero};
                                                                                 (x3,
                                                                                  x4) <- do {x4 <- gen_memberOIOO_x4;
                                                                                             let {x3 = Cons x20 x4};
                                                                                             return (x3,
                                                                                                     x4)};
                                                                                 return (x0,
                                                                                         x2,
                                                                                         x3)},
                                                                             do {(x0,
                                                                                  x2,
                                                                                  x6) <- memberOIOO x1 gen_memberOIOO_x18 gen_memberOIOO_x4 gen_memberOIOO_x5;
                                                                                 (x3,
                                                                                  x5) <- do {x5 <- gen_memberOIOO_x5;
                                                                                             let {x3 = Cons x5 x6};
                                                                                             return (x3,
                                                                                                     x5)};
                                                                                 return (x0,
                                                                                         x2,
                                                                                         x3)}]
solveOOI x2 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {let {x7 = Nil};
                                                                                                                (x0,
                                                                                                                 x1) <- _contrapositiveProveallOOII x2 x7 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                return (x0,
                                                                                                                        x1)}]
_contrapositiveProveallOOII x2 x3 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {(x0,
                                                                                                                                       x1) <- _input_clauseProveallOOII x2 x3 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                                      return (x0,
                                                                                                                                              x1)}]
_input_clauseProveallOOII x2 x3 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {let {x0 = Nil};
                                                                                                                                    let {x1 = x2};
                                                                                                                                    return (x0,
                                                                                                                                            x1)},
                                                                                                                                do {(x4,
                                                                                                                                     x5) <- case x2 of
                                                                                                                                            {Cons y4
                                                                                                                                                  y5 -> return (y4,
                                                                                                                                                                y5);
                                                                                                                                             _ -> mzero};
                                                                                                                                    let {x8 = x4};
                                                                                                                                    (x6,
                                                                                                                                     x1) <- _proveIIOOI x3 x4 x5 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                                    let {x0 = Cons x8 x6};
                                                                                                                                    return (x0,
                                                                                                                                            x1)}]
_proveIIOOI x0 x1 x4 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {(x2,
                                                                                                                          x3) <- memberOOII x4 x0;
                                                                                                                         return (x2,
                                                                                                                                 x3)},
                                                                                                                     do {let {x16 = x1};
                                                                                                                         let {x17 = x4};
                                                                                                                         let {x15 = Cons x16 x17};
                                                                                                                         (x2,
                                                                                                                          x3,
                                                                                                                          x9) <- _contrapositiveProveallOOIO x4 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                         x10 <- case x9 of
                                                                                                                                {Cons y10
                                                                                                                                      y0 -> do {guard (x0 == y0);
                                                                                                                                                return y10};
                                                                                                                                 _ -> mzero};
                                                                                                                         x11 <- case x10 of
                                                                                                                                {Pos y11 -> return y11;
                                                                                                                                 _ -> mzero};
                                                                                                                         (x12,
                                                                                                                          x14) <- case x11 of
                                                                                                                                  {App y12
                                                                                                                                       y14
                                                                                                                                       y15 -> do {guard (x15 == y15);
                                                                                                                                                  return (y12,
                                                                                                                                                          y14)};
                                                                                                                                   _ -> mzero};
                                                                                                                         guard (x14 == x3);
                                                                                                                         x13 <- case x12 of
                                                                                                                                {Cons y1
                                                                                                                                      y13 -> do {guard (x1 == y1);
                                                                                                                                                 return y13};
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x13 == x2);
                                                                                                                         return (x2,
                                                                                                                                 x3)}]
_contrapositiveProveallOOIO x2 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {(x0,
                                                                                                                                    x1,
                                                                                                                                    x3) <- _input_clauseProveallOOIO x2 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                                   return (x0,
                                                                                                                                           x1,
                                                                                                                                           x3)}]
_input_clauseProveallOOIO x2 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {let {x0 = Nil};
                                                                                                                                 let {x1 = x2};
                                                                                                                                 x3 <- gen__input_clauseProveallOOIO_x3;
                                                                                                                                 return (x0,
                                                                                                                                         x1,
                                                                                                                                         x3)},
                                                                                                                             do {(x4,
                                                                                                                                  x5) <- case x2 of
                                                                                                                                         {Cons y4
                                                                                                                                               y5 -> return (y4,
                                                                                                                                                             y5);
                                                                                                                                          _ -> mzero};
                                                                                                                                 let {x8 = x4};
                                                                                                                                 (x3,
                                                                                                                                  x6,
                                                                                                                                  x1) <- _proveOIOOI x4 x5 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                                 let {x0 = Cons x8 x6};
                                                                                                                                 return (x0,
                                                                                                                                         x1,
                                                                                                                                         x3)}]
_proveOIOOI x1 x4 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {(x2,
                                                                                                                       x3,
                                                                                                                       x0) <- memberOOIO x4 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                      return (x0,
                                                                                                                              x2,
                                                                                                                              x3)},
                                                                                                                  do {let {x16 = x1};
                                                                                                                      let {x17 = x4};
                                                                                                                      let {x15 = Cons x16 x17};
                                                                                                                      (x2,
                                                                                                                       x3,
                                                                                                                       x9) <- _contrapositiveProveallOOIO x4 gen__input_clauseProveallOOIO_x3 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                                                      (x10,
                                                                                                                       x0) <- case x9 of
                                                                                                                              {Cons y10
                                                                                                                                    y0 -> return (y10,
                                                                                                                                                  y0);
                                                                                                                               _ -> mzero};
                                                                                                                      x11 <- case x10 of
                                                                                                                             {Pos y11 -> return y11;
                                                                                                                              _ -> mzero};
                                                                                                                      (x12,
                                                                                                                       x14) <- case x11 of
                                                                                                                               {App y12
                                                                                                                                    y14
                                                                                                                                    y15 -> do {guard (x15 == y15);
                                                                                                                                               return (y12,
                                                                                                                                                       y14)};
                                                                                                                                _ -> mzero};
                                                                                                                      guard (x14 == x3);
                                                                                                                      x13 <- case x12 of
                                                                                                                             {Cons y1
                                                                                                                                   y13 -> do {guard (x1 == y1);
                                                                                                                                              return y13};
                                                                                                                              _ -> mzero};
                                                                                                                      guard (x13 == x2);
                                                                                                                      return (x0,
                                                                                                                              x2,
                                                                                                                              x3)}]
memberOOII x2 x3 = msum [do {(x20, x4) <- case x3 of
                                          {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                             let {x18 = x20};
                             x19 <- case x18 of
                                    {Neg y19 -> return y19; _ -> mzero};
                             (x0, x1) <- case x19 of
                                         {App y0 y1 y2 -> do {guard (x2 == y2); return (y0, y1)};
                                          _ -> mzero};
                             return (x0, x1)},
                         do {(x5, x6) <- case x3 of
                                         {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                             (x0, x1) <- memberOOII x2 x6;
                             return (x0, x1)}]
memberOOIO x2 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5 = msum [do {(x20,
                                                                                  x18) <- do {x18 <- gen_memberOOIO_x18;
                                                                                              return (x18,
                                                                                                      x18)};
                                                                                 x19 <- case x18 of
                                                                                        {Neg y19 -> return y19;
                                                                                         _ -> mzero};
                                                                                 (x0,
                                                                                  x1) <- case x19 of
                                                                                         {App y0
                                                                                              y1
                                                                                              y2 -> do {guard (x2 == y2);
                                                                                                        return (y0,
                                                                                                                y1)};
                                                                                          _ -> mzero};
                                                                                 (x3,
                                                                                  x4) <- do {x4 <- gen_memberOOIO_x4;
                                                                                             let {x3 = Cons x20 x4};
                                                                                             return (x3,
                                                                                                     x4)};
                                                                                 return (x0,
                                                                                         x1,
                                                                                         x3)},
                                                                             do {(x0,
                                                                                  x1,
                                                                                  x6) <- memberOOIO x2 gen_memberOOIO_x18 gen_memberOOIO_x4 gen_memberOOIO_x5;
                                                                                 (x3,
                                                                                  x5) <- do {x5 <- gen_memberOOIO_x5;
                                                                                             let {x3 = Cons x5 x6};
                                                                                             return (x3,
                                                                                                     x5)};
                                                                                 return (x0,
                                                                                         x1,
                                                                                         x3)}]
solveOOO gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__input_clauseProveallOOOI_x2 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {let {x7 = Nil};
                                                                                                                                                                                                                     (x0,
                                                                                                                                                                                                                      x1,
                                                                                                                                                                                                                      x2) <- _contrapositiveProveallOOOI x7 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__input_clauseProveallOOOI_x2 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                                                     return (x0,
                                                                                                                                                                                                                             x1,
                                                                                                                                                                                                                             x2)}]
_contrapositiveProveallOOOI x3 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__input_clauseProveallOOOI_x2 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x0,
                                                                                                                                                                                                                                            x1,
                                                                                                                                                                                                                                            x2) <- _input_clauseProveallOOOI x3 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__input_clauseProveallOOOI_x2 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                                                                           return (x0,
                                                                                                                                                                                                                                                   x1,
                                                                                                                                                                                                                                                   x2)}]
_input_clauseProveallOOOI x3 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__input_clauseProveallOOOI_x2 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {let {x0 = Nil};
                                                                                                                                                                                                                                         (x1,
                                                                                                                                                                                                                                          x2) <- do {x2 <- gen__input_clauseProveallOOOI_x2;
                                                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                                                             x2)};
                                                                                                                                                                                                                                         return (x0,
                                                                                                                                                                                                                                                 x1,
                                                                                                                                                                                                                                                 x2)},
                                                                                                                                                                                                                                     do {(x4,
                                                                                                                                                                                                                                          x6,
                                                                                                                                                                                                                                          x1,
                                                                                                                                                                                                                                          x5) <- _proveIOOOO x3 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                                                                         let {x2 = Cons x4 x5};
                                                                                                                                                                                                                                         let {x8 = x4};
                                                                                                                                                                                                                                         let {x0 = Cons x8 x6};
                                                                                                                                                                                                                                         return (x0,
                                                                                                                                                                                                                                                 x1,
                                                                                                                                                                                                                                                 x2)}]
_proveIOOOO x0 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen__proveIOOOO_x1 gen__proveIOOOO_x2 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5 = msum [do {(x2,
                                                                                                                                                                                           x3,
                                                                                                                                                                                           x4) <- memberOOOI x0;
                                                                                                                                                                                          x1 <- gen__proveIOOOO_x1;
                                                                                                                                                                                          return (x1,
                                                                                                                                                                                                  x2,
                                                                                                                                                                                                  x3,
                                                                                                                                                                                                  x4)},
                                                                                                                                                                                      do {(x13,
                                                                                                                                                                                           x2) <- do {x2 <- gen__proveIOOOO_x2;
                                                                                                                                                                                                      return (x2,
                                                                                                                                                                                                              x2)};
                                                                                                                                                                                          (x3,
                                                                                                                                                                                           x4,
                                                                                                                                                                                           x9) <- _contrapositiveProveallIOOO x2 gen__input_clauseProveallIOOO_x2 gen__input_clauseProveallIOOO_x3 gen_memberIOOO_x18 gen_memberIOOO_x4 gen_memberIOOO_x5;
                                                                                                                                                                                          x10 <- case x9 of
                                                                                                                                                                                                 {Cons y10
                                                                                                                                                                                                       y0 -> do {guard (x0 == y0);
                                                                                                                                                                                                                 return y10};
                                                                                                                                                                                                  _ -> mzero};
                                                                                                                                                                                          x11 <- case x10 of
                                                                                                                                                                                                 {Pos y11 -> return y11;
                                                                                                                                                                                                  _ -> mzero};
                                                                                                                                                                                          (x12,
                                                                                                                                                                                           x14,
                                                                                                                                                                                           x15) <- case x11 of
                                                                                                                                                                                                   {App y12
                                                                                                                                                                                                        y14
                                                                                                                                                                                                        y15 -> return (y12,
                                                                                                                                                                                                                       y14,
                                                                                                                                                                                                                       y15);
                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                          guard (x14 == x3);
                                                                                                                                                                                          x1 <- case x12 of
                                                                                                                                                                                                {Cons y1
                                                                                                                                                                                                      y13 -> do {guard (x13 == y13);
                                                                                                                                                                                                                 return y1};
                                                                                                                                                                                                 _ -> mzero};
                                                                                                                                                                                          (x16,
                                                                                                                                                                                           x17) <- case x15 of
                                                                                                                                                                                                   {Cons y16
                                                                                                                                                                                                         y17 -> return (y16,
                                                                                                                                                                                                                        y17);
                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                          guard (x16 == x1);
                                                                                                                                                                                          guard (x17 == x4);
                                                                                                                                                                                          return (x1,
                                                                                                                                                                                                  x2,
                                                                                                                                                                                                  x3,
                                                                                                                                                                                                  x4)}]
memberOOOI x3 = msum [do {(x20, x4) <- case x3 of
                                       {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                          let {x18 = x20};
                          x19 <- case x18 of
                                 {Neg y19 -> return y19; _ -> mzero};
                          (x0, x1, x2) <- case x19 of
                                          {App y0 y1 y2 -> return (y0, y1, y2); _ -> mzero};
                          return (x0, x1, x2)},
                      do {(x5, x6) <- case x3 of
                                      {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                          (x0, x1, x2) <- memberOOOI x6;
                          return (x0, x1, x2)}]