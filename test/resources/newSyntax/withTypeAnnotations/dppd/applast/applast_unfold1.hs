module Applast where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    deriving (Show, Eq)
applastoIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                 guard (x0 == Nil);
                                 return ()},
                             do {let {x7 = Nil};
                                 (x6, x3) <- case x0 of
                                             {Cons y6 y3 -> return (y6, y3); _ -> mzero};
                                 guard (x6 == x2);
                                 _appendoIII x1 x3 x7;
                                 return ()},
                             do {(x4, x3) <- case x0 of
                                             {Cons y4 y3 -> return (y4, y3); _ -> mzero};
                                 appendoLastoIII x1 x2 x3;
                                 return ()}]
_appendoIII x0 x1 x2 = msum [do {let {x11 = Nil};
                                 (x12, x13) <- case x2 of
                                               {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                                 guard (x12 == x0);
                                 guard (x13 == x11);
                                 guard (x1 == Nil);
                                 return ()},
                             do {(x3, x4) <- case x2 of
                                             {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                 (x14, x5) <- case x1 of
                                              {Cons y14 y5 -> return (y14, y5); _ -> mzero};
                                 guard (x14 == x3);
                                 _appendoIII x0 x5 x4;
                                 return ()}]
appendoLastoIII x0 x1 x2 = msum [do {let {x9 = Nil};
                                     let {x8 = Cons x0 x9};
                                     lastoII x1 x8;
                                     guard (x2 == Nil);
                                     return ()},
                                 do {(x3, x4) <- case x2 of
                                                 {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                     x5 <- _appendoIIO x0 x4;
                                     let {x10 = Cons x3 x5};
                                     lastoII x1 x10;
                                     return ()}]
_appendoIIO x0 x1 = msum [do {let {x11 = Nil};
                              guard (x1 == Nil);
                              let {x12 = x0};
                              let {x13 = x11};
                              let {x2 = Cons x12 x13};
                              return x2},
                          do {(x14, x5) <- case x1 of
                                           {Cons y14 y5 -> return (y14, y5); _ -> mzero};
                              let {x3 = x14};
                              x4 <- _appendoIIO x0 x5;
                              let {x2 = Cons x3 x4};
                              return x2}]
applastoIIO x0 x1 = msum [do {guard (x0 == Nil);
                              let {x2 = x1};
                              return x2},
                          do {let {x7 = Nil};
                              (x6, x3) <- case x0 of
                                          {Cons y6 y3 -> return (y6, y3); _ -> mzero};
                              _appendoIII x1 x3 x7;
                              let {x2 = x6};
                              return x2},
                          do {(x4, x3) <- case x0 of
                                          {Cons y4 y3 -> return (y4, y3); _ -> mzero};
                              x2 <- appendoLastoIOI x1 x3;
                              return x2}]
appendoLastoIOI x0 x2 = msum [do {let {x9 = Nil};
                                  let {x8 = Cons x0 x9};
                                  guard (x2 == Nil);
                                  x1 <- lastoOI x8;
                                  return x1},
                              do {(x3, x4) <- case x2 of
                                              {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                  x5 <- _appendoIIO x0 x4;
                                  let {x10 = Cons x3 x5};
                                  x1 <- lastoOI x10;
                                  return x1}]
applastoIOI x0 x2 gen_lastoIO_x2 = msum [do {guard (x0 == Nil);
                                             let {x1 = x2};
                                             return x1},
                                         do {let {x7 = Nil};
                                             (x6, x3) <- case x0 of
                                                         {Cons y6 y3 -> return (y6, y3);
                                                          _ -> mzero};
                                             guard (x6 == x2);
                                             x1 <- _appendoOII x3 x7;
                                             return x1},
                                         do {(x4, x3) <- case x0 of
                                                         {Cons y4 y3 -> return (y4, y3);
                                                          _ -> mzero};
                                             x1 <- appendoLastoOII x2 x3 gen_lastoIO_x2;
                                             return x1}]
_appendoOII x1 x2 = msum [do {let {x11 = Nil};
                              (x12, x13) <- case x2 of
                                            {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                              guard (x13 == x11);
                              guard (x1 == Nil);
                              let {x0 = x12};
                              return x0},
                          do {(x3, x4) <- case x2 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              (x14, x5) <- case x1 of
                                           {Cons y14 y5 -> return (y14, y5); _ -> mzero};
                              guard (x14 == x3);
                              x0 <- _appendoOII x5 x4;
                              return x0}]
appendoLastoOII x1 x2 gen_lastoIO_x2 = msum [do {let {x9 = Nil};
                                                 guard (x2 == Nil);
                                                 x8 <- lastoIO x1 gen_lastoIO_x2;
                                                 x0 <- case x8 of
                                                       {Cons y0 y9 -> do {guard (x9 == y9);
                                                                          return y0};
                                                        _ -> mzero};
                                                 return x0},
                                             do {(x3, x4) <- case x2 of
                                                             {Cons y3 y4 -> return (y3, y4);
                                                              _ -> mzero};
                                                 x10 <- lastoIO x1 gen_lastoIO_x2;
                                                 x5 <- case x10 of
                                                       {Cons y3 y5 -> do {guard (x3 == y3);
                                                                          return y5};
                                                        _ -> mzero};
                                                 x0 <- _appendoOII x4 x5;
                                                 return x0}]
applastoIOO x0 gen__appendoOIO_x0 gen_appendoLastoOOI_x0 gen_applastoIOO_x2 = msum [do {guard (x0 == Nil);
                                                                                        (x1,
                                                                                         x2) <- do {x2 <- gen_applastoIOO_x2;
                                                                                                    return (x2,
                                                                                                            x2)};
                                                                                        return (x1,
                                                                                                x2)},
                                                                                    do {let {x7 = Nil};
                                                                                        (x6,
                                                                                         x3) <- case x0 of
                                                                                                {Cons y6
                                                                                                      y3 -> return (y6,
                                                                                                                    y3);
                                                                                                 _ -> mzero};
                                                                                        let {x2 = x6};
                                                                                        x1 <- _appendoOII x3 x7;
                                                                                        return (x1,
                                                                                                x2)},
                                                                                    do {(x4,
                                                                                         x3) <- case x0 of
                                                                                                {Cons y4
                                                                                                      y3 -> return (y4,
                                                                                                                    y3);
                                                                                                 _ -> mzero};
                                                                                        (x1,
                                                                                         x2) <- appendoLastoOOI x3 gen__appendoOIO_x0 gen_appendoLastoOOI_x0;
                                                                                        return (x1,
                                                                                                x2)}]
appendoLastoOOI x2 gen__appendoOIO_x0 gen_appendoLastoOOI_x0 = msum [do {let {x9 = Nil};
                                                                         guard (x2 == Nil);
                                                                         (x8,
                                                                          x0) <- do {x0 <- gen_appendoLastoOOI_x0;
                                                                                     let {x8 = Cons x0 x9};
                                                                                     return (x8,
                                                                                             x0)};
                                                                         x1 <- lastoOI x8;
                                                                         return (x0, x1)},
                                                                     do {(x3, x4) <- case x2 of
                                                                                     {Cons y3
                                                                                           y4 -> return (y3,
                                                                                                         y4);
                                                                                      _ -> mzero};
                                                                         (x0,
                                                                          x5) <- _appendoOIO x4 gen__appendoOIO_x0;
                                                                         let {x10 = Cons x3 x5};
                                                                         x1 <- lastoOI x10;
                                                                         return (x0, x1)}]
_appendoOIO x1 gen__appendoOIO_x0 = msum [do {let {x11 = Nil};
                                              guard (x1 == Nil);
                                              let {x13 = x11};
                                              (x12, x0) <- do {x0 <- gen__appendoOIO_x0;
                                                               return (x0, x0)};
                                              let {x2 = Cons x12 x13};
                                              return (x0, x2)},
                                          do {(x14, x5) <- case x1 of
                                                           {Cons y14 y5 -> return (y14, y5);
                                                            _ -> mzero};
                                              let {x3 = x14};
                                              (x0, x4) <- _appendoOIO x5 gen__appendoOIO_x0;
                                              let {x2 = Cons x3 x4};
                                              return (x0, x2)}]
applastoOII x1 x2 gen_applastoOII_x4 gen_lastoIO_x2 = msum [do {guard (x1 == x2);
                                                                let {x0 = Nil};
                                                                return x0},
                                                            do {let {x7 = Nil};
                                                                let {x6 = x2};
                                                                x3 <- _appendoIOI x1 x7;
                                                                let {x0 = Cons x6 x3};
                                                                return x0},
                                                            do {x3 <- appendoLastoIIO x1 x2 gen_lastoIO_x2;
                                                                (x0,
                                                                 x4) <- do {x4 <- gen_applastoOII_x4;
                                                                            let {x0 = Cons x4 x3};
                                                                            return (x0, x4)};
                                                                return x0}]
_appendoIOI x0 x2 = msum [do {let {x11 = Nil};
                              let {x1 = Nil};
                              (x12, x13) <- case x2 of
                                            {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                              guard (x12 == x0);
                              guard (x13 == x11);
                              return x1},
                          do {(x3, x4) <- case x2 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              let {x14 = x3};
                              x5 <- _appendoIOI x0 x4;
                              let {x1 = Cons x14 x5};
                              return x1}]
appendoLastoIIO x0 x1 gen_lastoIO_x2 = msum [do {let {x2 = Nil};
                                                 let {x9 = Nil};
                                                 let {x8 = Cons x0 x9};
                                                 lastoII x1 x8;
                                                 return x2},
                                             do {x10 <- lastoIO x1 gen_lastoIO_x2;
                                                 (x3, x5) <- case x10 of
                                                             {Cons y3 y5 -> return (y3, y5);
                                                              _ -> mzero};
                                                 x4 <- _appendoIOI x0 x5;
                                                 let {x2 = Cons x3 x4};
                                                 return x2}]
applastoOIO x1 gen__appendoIOO_x3 gen_appendoLastoIOO_x3 gen_applastoOIO_x2 gen_applastoOIO_x4 = msum [do {let {x0 = Nil};
                                                                                                           let {x2 = x1};
                                                                                                           return (x0,
                                                                                                                   x2)},
                                                                                                       do {let {x7 = Nil};
                                                                                                           x3 <- _appendoIOI x1 x7;
                                                                                                           (x6,
                                                                                                            x2) <- do {x2 <- gen_applastoOIO_x2;
                                                                                                                       return (x2,
                                                                                                                               x2)};
                                                                                                           let {x0 = Cons x6 x3};
                                                                                                           return (x0,
                                                                                                                   x2)},
                                                                                                       do {(x2,
                                                                                                            x3) <- appendoLastoIOO x1 gen__appendoIOO_x3 gen_appendoLastoIOO_x3;
                                                                                                           (x0,
                                                                                                            x4) <- do {x4 <- gen_applastoOIO_x4;
                                                                                                                       let {x0 = Cons x4 x3};
                                                                                                                       return (x0,
                                                                                                                               x4)};
                                                                                                           return (x0,
                                                                                                                   x2)}]
appendoLastoIOO x0 gen__appendoIOO_x3 gen_appendoLastoIOO_x3 = msum [do {let {x2 = Nil};
                                                                         let {x9 = Nil};
                                                                         let {x8 = Cons x0 x9};
                                                                         x1 <- lastoOI x8;
                                                                         return (x1, x2)},
                                                                     do {(x4,
                                                                          x5) <- _appendoIOO x0 gen__appendoIOO_x3;
                                                                         (x2,
                                                                          x3) <- do {x3 <- gen_appendoLastoIOO_x3;
                                                                                     let {x2 = Cons x3 x4};
                                                                                     return (x2,
                                                                                             x3)};
                                                                         let {x10 = Cons x3 x5};
                                                                         x1 <- lastoOI x10;
                                                                         return (x1, x2)}]
_appendoIOO x0 gen__appendoIOO_x3 = msum [do {let {x11 = Nil};
                                              let {x1 = Nil};
                                              let {x12 = x0};
                                              let {x13 = x11};
                                              let {x2 = Cons x12 x13};
                                              return (x1, x2)},
                                          do {(x5, x4) <- _appendoIOO x0 gen__appendoIOO_x3;
                                              (x14, x3) <- do {x3 <- gen__appendoIOO_x3;
                                                               return (x3, x3)};
                                              let {x2 = Cons x3 x4};
                                              let {x1 = Cons x14 x5};
                                              return (x1, x2)}]
applastoOOI x2 gen_applastoOOI_x4 gen_lastoIO_x2 = msum [do {let {x0 = Nil};
                                                             let {x1 = x2};
                                                             return (x0, x1)},
                                                         do {let {x7 = Nil};
                                                             let {x6 = x2};
                                                             (x1, x3) <- _appendoOOI x7;
                                                             let {x0 = Cons x6 x3};
                                                             return (x0, x1)},
                                                         do {(x1,
                                                              x3) <- appendoLastoOIO x2 gen_lastoIO_x2;
                                                             (x0,
                                                              x4) <- do {x4 <- gen_applastoOOI_x4;
                                                                         let {x0 = Cons x4 x3};
                                                                         return (x0, x4)};
                                                             return (x0, x1)}]
_appendoOOI x2 = msum [do {let {x11 = Nil};
                           let {x1 = Nil};
                           (x12, x13) <- case x2 of
                                         {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                           guard (x13 == x11);
                           let {x0 = x12};
                           return (x0, x1)},
                       do {(x3, x4) <- case x2 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           let {x14 = x3};
                           (x0, x5) <- _appendoOOI x4;
                           let {x1 = Cons x14 x5};
                           return (x0, x1)}]
appendoLastoOIO x1 gen_lastoIO_x2 = msum [do {let {x2 = Nil};
                                              let {x9 = Nil};
                                              x8 <- lastoIO x1 gen_lastoIO_x2;
                                              x0 <- case x8 of
                                                    {Cons y0 y9 -> do {guard (x9 == y9); return y0};
                                                     _ -> mzero};
                                              return (x0, x2)},
                                          do {x10 <- lastoIO x1 gen_lastoIO_x2;
                                              (x3, x5) <- case x10 of
                                                          {Cons y3 y5 -> return (y3, y5);
                                                           _ -> mzero};
                                              (x0, x4) <- _appendoOOI x5;
                                              let {x2 = Cons x3 x4};
                                              return (x0, x2)}]
applastoOOO gen__appendoOOO_x0 gen__appendoOOO_x3 gen_appendoLastoOOO_x0 gen_appendoLastoOOO_x3 gen_applastoOOO_x2 gen_applastoOOO_x4 = msum [do {let {x0 = Nil};
                                                                                                                                                  (x1,
                                                                                                                                                   x2) <- do {x2 <- gen_applastoOOO_x2;
                                                                                                                                                              return (x2,
                                                                                                                                                                      x2)};
                                                                                                                                                  return (x0,
                                                                                                                                                          x1,
                                                                                                                                                          x2)},
                                                                                                                                              do {let {x7 = Nil};
                                                                                                                                                  (x1,
                                                                                                                                                   x3) <- _appendoOOI x7;
                                                                                                                                                  (x6,
                                                                                                                                                   x2) <- do {x2 <- gen_applastoOOO_x2;
                                                                                                                                                              return (x2,
                                                                                                                                                                      x2)};
                                                                                                                                                  let {x0 = Cons x6 x3};
                                                                                                                                                  return (x0,
                                                                                                                                                          x1,
                                                                                                                                                          x2)},
                                                                                                                                              do {(x1,
                                                                                                                                                   x2,
                                                                                                                                                   x3) <- appendoLastoOOO gen__appendoOOO_x0 gen__appendoOOO_x3 gen_appendoLastoOOO_x0 gen_appendoLastoOOO_x3;
                                                                                                                                                  (x0,
                                                                                                                                                   x4) <- do {x4 <- gen_applastoOOO_x4;
                                                                                                                                                              let {x0 = Cons x4 x3};
                                                                                                                                                              return (x0,
                                                                                                                                                                      x4)};
                                                                                                                                                  return (x0,
                                                                                                                                                          x1,
                                                                                                                                                          x2)}]
appendoLastoOOO gen__appendoOOO_x0 gen__appendoOOO_x3 gen_appendoLastoOOO_x0 gen_appendoLastoOOO_x3 = msum [do {let {x2 = Nil};
                                                                                                                let {x9 = Nil};
                                                                                                                (x8,
                                                                                                                 x0) <- do {x0 <- gen_appendoLastoOOO_x0;
                                                                                                                            let {x8 = Cons x0 x9};
                                                                                                                            return (x8,
                                                                                                                                    x0)};
                                                                                                                x1 <- lastoOI x8;
                                                                                                                return (x0,
                                                                                                                        x1,
                                                                                                                        x2)},
                                                                                                            do {(x0,
                                                                                                                 x4,
                                                                                                                 x5) <- _appendoOOO gen__appendoOOO_x0 gen__appendoOOO_x3;
                                                                                                                (x2,
                                                                                                                 x3) <- do {x3 <- gen_appendoLastoOOO_x3;
                                                                                                                            let {x2 = Cons x3 x4};
                                                                                                                            return (x2,
                                                                                                                                    x3)};
                                                                                                                let {x10 = Cons x3 x5};
                                                                                                                x1 <- lastoOI x10;
                                                                                                                return (x0,
                                                                                                                        x1,
                                                                                                                        x2)}]
_appendoOOO gen__appendoOOO_x0 gen__appendoOOO_x3 = msum [do {let {x11 = Nil};
                                                              let {x1 = Nil};
                                                              let {x13 = x11};
                                                              (x12,
                                                               x0) <- do {x0 <- gen__appendoOOO_x0;
                                                                          return (x0, x0)};
                                                              let {x2 = Cons x12 x13};
                                                              return (x0, x1, x2)},
                                                          do {(x0,
                                                               x5,
                                                               x4) <- _appendoOOO gen__appendoOOO_x0 gen__appendoOOO_x3;
                                                              (x14,
                                                               x3) <- do {x3 <- gen__appendoOOO_x3;
                                                                          return (x3, x3)};
                                                              let {x2 = Cons x3 x4};
                                                              let {x1 = Cons x14 x5};
                                                              return (x0, x1, x2)}]
lastoII x0 x1 = msum [do {let {x15 = Nil};
                          (x16, x17) <- case x1 of
                                        {Cons y16 y17 -> return (y16, y17); _ -> mzero};
                          guard (x16 == x0);
                          guard (x17 == x15);
                          return ()},
                      do {(x2, x3) <- case x1 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          lastoII x0 x3;
                          return ()}]
lastoIO x0 gen_lastoIO_x2 = msum [do {let {x15 = Nil};
                                      let {x16 = x0};
                                      let {x17 = x15};
                                      let {x1 = Cons x16 x17};
                                      return x1},
                                  do {x3 <- lastoIO x0 gen_lastoIO_x2;
                                      (x1, x2) <- do {x2 <- gen_lastoIO_x2;
                                                      let {x1 = Cons x2 x3};
                                                      return (x1, x2)};
                                      return x1}]
lastoOI x1 = msum [do {let {x15 = Nil};
                       (x16, x17) <- case x1 of
                                     {Cons y16 y17 -> return (y16, y17); _ -> mzero};
                       guard (x17 == x15);
                       let {x0 = x16};
                       return x0},
                   do {(x2, x3) <- case x1 of
                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                       x0 <- lastoOI x3;
                       return x0}]