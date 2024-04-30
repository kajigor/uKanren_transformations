module Deforestation2_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrI x0 = msum [do {(x11, x12) <- case x0 of
                                 {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                   let {x1 = x11};
                   let {x10 = x1};
                   let {x9 = x12};
                   x2 <- case x9 of
                         {Cons y10 y2 -> do {guard (x10 == y10); return y2}; _ -> mzero};
                   rRII x1 x2;
                   return ()},
               do {(x3, x14) <- case x0 of
                                {Cons y3 y14 -> return (y3, y14); _ -> mzero};
                   let {x13 = x14};
                   (x4, x5) <- case x13 of
                               {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                   neqRRIII x3 x4 x5;
                   return ()}]
neqRRIII x0 x1 x2 = msum [do {let {x62 = O};
                              x3 <- case x1 of
                                    {S y3 -> return y3; _ -> mzero};
                              let {x64 = S x3};
                              let {x63 = Cons x64 x2};
                              rRII x62 x63;
                              guard (x0 == O);
                              return ()},
                          do {let {x67 = O};
                              let {x66 = Cons x67 x2};
                              guard (x1 == O);
                              x4 <- case x0 of
                                    {S y4 -> return y4; _ -> mzero};
                              let {x65 = S x4};
                              rRII x65 x66;
                              return ()},
                          do {x5 <- case x1 of
                                    {S y5 -> return y5; _ -> mzero};
                              let {x70 = S x5};
                              let {x69 = Cons x70 x2};
                              x71 <- case x0 of
                                     {S y71 -> return y71; _ -> mzero};
                              let {x6 = x71};
                              neqII x6 x5;
                              let {x68 = S x6};
                              rRII x68 x69;
                              return ()}]
neqII x0 x1 = msum [do {x2 <- case x1 of
                              {S y2 -> return y2; _ -> mzero};
                        guard (x0 == O);
                        return ()},
                    do {guard (x1 == O);
                        x3 <- case x0 of
                              {S y3 -> return y3; _ -> mzero};
                        return ()},
                    do {x4 <- case x1 of
                              {S y4 -> return y4; _ -> mzero};
                        x5 <- case x0 of
                              {S y5 -> return y5; _ -> mzero};
                        neqII x5 x4;
                        return ()}]
rRII x0 x1 = msum [do {let {x23 = O};
                       let {x22 = S x23};
                       let {x25 = O};
                       let {x29 = O};
                       let {x28 = S x29};
                       let {x27 = S x28};
                       let {x31 = O};
                       let {x33 = O};
                       let {x34 = Nil};
                       let {x32 = Cons x33 x34};
                       let {x30 = Cons x31 x32};
                       let {x26 = Cons x27 x30};
                       let {x24 = Cons x25 x26};
                       let {x21 = Cons x22 x24};
                       (x17, x18) <- case x1 of
                                     {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                       let {x2 = x17};
                       let {x16 = x2};
                       let {x15 = x18};
                       x3 <- case x15 of
                             {Cons y16 y3 -> do {guard (x16 == y16); return y3}; _ -> mzero};
                       x4 <- rIO x3;
                       let {x20 = Cons x2 x4};
                       let {x19 = Cons x0 x20};
                       rII x19 x21;
                       return ()},
                   do {let {x42 = O};
                       let {x41 = S x42};
                       let {x44 = O};
                       let {x48 = O};
                       let {x47 = S x48};
                       let {x46 = S x47};
                       let {x50 = O};
                       let {x52 = O};
                       let {x53 = Nil};
                       let {x51 = Cons x52 x53};
                       let {x49 = Cons x50 x51};
                       let {x45 = Cons x46 x49};
                       let {x43 = Cons x44 x45};
                       let {x40 = Cons x41 x43};
                       (x5, x36) <- case x1 of
                                    {Cons y5 y36 -> return (y5, y36); _ -> mzero};
                       let {x35 = x36};
                       (x6, x7) <- case x35 of
                                   {Cons y6 y7 -> return (y6, y7); _ -> mzero};
                       neqII x5 x6;
                       let {x37 = Cons x6 x7};
                       x8 <- rIO x37;
                       let {x39 = Cons x5 x8};
                       let {x38 = Cons x0 x39};
                       rII x38 x40;
                       return ()}]
rII x0 x1 = msum [do {guard (x1 == Nil);
                      guard (x0 == Nil);
                      return ()},
                  do {(x2, x3) <- case x1 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      (x56, x57) <- case x0 of
                                    {Cons y56 y57 -> return (y56, y57); _ -> mzero};
                      guard (x56 == x2);
                      let {x55 = x2};
                      let {x54 = x57};
                      x4 <- case x54 of
                            {Cons y55 y4 -> do {guard (x55 == y55); return y4}; _ -> mzero};
                      rII x4 x3;
                      return ()},
                  do {(x5, x6) <- case x1 of
                                  {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                      (x59, x60) <- case x0 of
                                    {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                      guard (x59 == x5);
                      let {x58 = x60};
                      (x7, x8) <- case x58 of
                                  {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                      neqII x5 x7;
                      let {x61 = Cons x7 x8};
                      rII x61 x6;
                      return ()}]
rIO x0 = msum [do {let {x1 = Nil}; guard (x0 == Nil); return x1},
               do {(x56, x57) <- case x0 of
                                 {Cons y56 y57 -> return (y56, y57); _ -> mzero};
                   let {x2 = x56};
                   let {x55 = x2};
                   let {x54 = x57};
                   x4 <- case x54 of
                         {Cons y55 y4 -> do {guard (x55 == y55); return y4}; _ -> mzero};
                   x3 <- rIO x4;
                   let {x1 = Cons x2 x3};
                   return x1},
               do {(x59, x60) <- case x0 of
                                 {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                   let {x5 = x59};
                   let {x58 = x60};
                   (x7, x8) <- case x58 of
                               {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                   neqII x5 x7;
                   let {x61 = Cons x7 x8};
                   x6 <- rIO x61;
                   let {x1 = Cons x5 x6};
                   return x1}]
rrO gen_neqIO_x2 gen_rrO_x1 gen_rrO_x13 = msum [do {(x10,
                                                     x1) <- do {x1 <- gen_rrO_x1; return (x1, x1)};
                                                    let {x11 = x1};
                                                    x2 <- rRIO x1 gen_neqIO_x2;
                                                    let {x9 = Cons x10 x2};
                                                    let {x12 = x9};
                                                    let {x0 = Cons x11 x12};
                                                    return x0},
                                                do {(x14, x13) <- do {x13 <- gen_rrO_x13;
                                                                      return (x13, x13)};
                                                    (x4, x5) <- case x13 of
                                                                {Cons y4 y5 -> return (y4, y5);
                                                                 _ -> mzero};
                                                    x3 <- neqRROII x4 x5 gen_neqIO_x2;
                                                    let {x0 = Cons x3 x14};
                                                    return x0}]
neqRROII x1 x2 gen_neqIO_x2 = msum [do {let {x0 = O};
                                        let {x62 = O};
                                        x3 <- case x1 of
                                              {S y3 -> return y3; _ -> mzero};
                                        let {x64 = S x3};
                                        let {x63 = Cons x64 x2};
                                        rRII x62 x63;
                                        return x0},
                                    do {let {x67 = O};
                                        let {x66 = Cons x67 x2};
                                        guard (x1 == O);
                                        x65 <- rROI x66 gen_neqIO_x2;
                                        x4 <- case x65 of
                                              {S y4 -> return y4; _ -> mzero};
                                        let {x0 = S x4};
                                        return x0},
                                    do {x5 <- case x1 of
                                              {S y5 -> return y5; _ -> mzero};
                                        let {x70 = S x5};
                                        let {x69 = Cons x70 x2};
                                        x68 <- rROI x69 gen_neqIO_x2;
                                        x6 <- case x68 of
                                              {S y6 -> return y6; _ -> mzero};
                                        neqII x6 x5;
                                        let {x71 = x6};
                                        let {x0 = S x71};
                                        return x0}]
rRIO x0 gen_neqIO_x2 = msum [do {let {x23 = O};
                                 let {x22 = S x23};
                                 let {x25 = O};
                                 let {x29 = O};
                                 let {x28 = S x29};
                                 let {x27 = S x28};
                                 let {x31 = O};
                                 let {x33 = O};
                                 let {x34 = Nil};
                                 let {x32 = Cons x33 x34};
                                 let {x30 = Cons x31 x32};
                                 let {x26 = Cons x27 x30};
                                 let {x24 = Cons x25 x26};
                                 let {x21 = Cons x22 x24};
                                 x19 <- rOI x21 gen_neqIO_x2;
                                 x20 <- case x19 of
                                        {Cons y0 y20 -> do {guard (x0 == y0); return y20};
                                         _ -> mzero};
                                 (x2, x4) <- case x20 of
                                             {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                                 let {x16 = x2};
                                 let {x17 = x2};
                                 x3 <- rOI x4 gen_neqIO_x2;
                                 let {x15 = Cons x16 x3};
                                 let {x18 = x15};
                                 let {x1 = Cons x17 x18};
                                 return x1},
                             do {let {x42 = O};
                                 let {x41 = S x42};
                                 let {x44 = O};
                                 let {x48 = O};
                                 let {x47 = S x48};
                                 let {x46 = S x47};
                                 let {x50 = O};
                                 let {x52 = O};
                                 let {x53 = Nil};
                                 let {x51 = Cons x52 x53};
                                 let {x49 = Cons x50 x51};
                                 let {x45 = Cons x46 x49};
                                 let {x43 = Cons x44 x45};
                                 let {x40 = Cons x41 x43};
                                 x38 <- rOI x40 gen_neqIO_x2;
                                 x39 <- case x38 of
                                        {Cons y0 y39 -> do {guard (x0 == y0); return y39};
                                         _ -> mzero};
                                 (x5, x8) <- case x39 of
                                             {Cons y5 y8 -> return (y5, y8); _ -> mzero};
                                 x6 <- neqIO x5 gen_neqIO_x2;
                                 x37 <- rOI x8 gen_neqIO_x2;
                                 x7 <- case x37 of
                                       {Cons y6 y7 -> do {guard (x6 == y6); return y7}; _ -> mzero};
                                 let {x35 = Cons x6 x7};
                                 let {x36 = x35};
                                 let {x1 = Cons x5 x36};
                                 return x1}]
neqIO x0 gen_neqIO_x2 = msum [do {guard (x0 == O);
                                  (x1, x2) <- do {x2 <- gen_neqIO_x2;
                                                  let {x1 = S x2};
                                                  return (x1, x2)};
                                  return x1},
                              do {let {x1 = O};
                                  x3 <- case x0 of
                                        {S y3 -> return y3; _ -> mzero};
                                  return x1},
                              do {x5 <- case x0 of
                                        {S y5 -> return y5; _ -> mzero};
                                  x4 <- neqIO x5 gen_neqIO_x2;
                                  let {x1 = S x4};
                                  return x1}]
rOI x1 gen_neqIO_x2 = msum [do {let {x0 = Nil};
                                guard (x1 == Nil);
                                return x0},
                            do {(x2, x3) <- case x1 of
                                            {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                let {x55 = x2};
                                let {x56 = x2};
                                x4 <- rOI x3 gen_neqIO_x2;
                                let {x54 = Cons x55 x4};
                                let {x57 = x54};
                                let {x0 = Cons x56 x57};
                                return x0},
                            do {(x5, x6) <- case x1 of
                                            {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                let {x59 = x5};
                                x7 <- neqIO x5 gen_neqIO_x2;
                                x61 <- rOI x6 gen_neqIO_x2;
                                x8 <- case x61 of
                                      {Cons y7 y8 -> do {guard (x7 == y7); return y8}; _ -> mzero};
                                let {x58 = Cons x7 x8};
                                let {x60 = x58};
                                let {x0 = Cons x59 x60};
                                return x0}]
rROI x1 gen_neqIO_x2 = msum [do {let {x23 = O};
                                 let {x22 = S x23};
                                 let {x25 = O};
                                 let {x29 = O};
                                 let {x28 = S x29};
                                 let {x27 = S x28};
                                 let {x31 = O};
                                 let {x33 = O};
                                 let {x34 = Nil};
                                 let {x32 = Cons x33 x34};
                                 let {x30 = Cons x31 x32};
                                 let {x26 = Cons x27 x30};
                                 let {x24 = Cons x25 x26};
                                 let {x21 = Cons x22 x24};
                                 (x17, x18) <- case x1 of
                                               {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                                 let {x2 = x17};
                                 let {x16 = x2};
                                 let {x15 = x18};
                                 x3 <- case x15 of
                                       {Cons y16 y3 -> do {guard (x16 == y16); return y3};
                                        _ -> mzero};
                                 x4 <- rIO x3;
                                 let {x20 = Cons x2 x4};
                                 x19 <- rOI x21 gen_neqIO_x2;
                                 x0 <- case x19 of
                                       {Cons y0 y20 -> do {guard (x20 == y20); return y0};
                                        _ -> mzero};
                                 return x0},
                             do {let {x42 = O};
                                 let {x41 = S x42};
                                 let {x44 = O};
                                 let {x48 = O};
                                 let {x47 = S x48};
                                 let {x46 = S x47};
                                 let {x50 = O};
                                 let {x52 = O};
                                 let {x53 = Nil};
                                 let {x51 = Cons x52 x53};
                                 let {x49 = Cons x50 x51};
                                 let {x45 = Cons x46 x49};
                                 let {x43 = Cons x44 x45};
                                 let {x40 = Cons x41 x43};
                                 (x5, x36) <- case x1 of
                                              {Cons y5 y36 -> return (y5, y36); _ -> mzero};
                                 let {x35 = x36};
                                 (x6, x7) <- case x35 of
                                             {Cons y6 y7 -> return (y6, y7); _ -> mzero};
                                 neqII x5 x6;
                                 let {x37 = Cons x6 x7};
                                 x8 <- rIO x37;
                                 let {x39 = Cons x5 x8};
                                 x38 <- rOI x40 gen_neqIO_x2;
                                 x0 <- case x38 of
                                       {Cons y0 y39 -> do {guard (x39 == y39); return y0};
                                        _ -> mzero};
                                 return x0}]