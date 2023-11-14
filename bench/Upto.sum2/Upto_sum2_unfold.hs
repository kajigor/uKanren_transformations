module Upto_sum2_unfold where

import Stream
import Control.Monad
import Term

sumtrsquaretrIIII x0 x1 x2 x3 = msum [do {let {x10 = O};
                                          guard (x0 == O);
                                          x4 <- squareAddIIO x2 x3;
                                          x5 <- squareAddIIO x10 x4;
                                          squareII x1 x5;
                                          return ()},
                                      do {x6 <- case x3 of
                                                {S y6 -> return y6; _ -> mzero};
                                          x11 <- case x0 of
                                                 {S y11 -> return y11; _ -> mzero};
                                          let {x7 = x11};
                                          x8 <- squareIO x2;
                                          x9 <- addMultiplySquareAddAddAddIIOI x7 x8 x6;
                                          squareII x1 x9;
                                          return ()}]
addMultiplySquareAddAddAddIIOI x0 x1 x3 = msum [do {let {x21 = O};
                                                    guard (x0 == O);
                                                    x2 <- squareAddAddIIOI x3 x1 x21;
                                                    return x2},
                                                do {x4 <- case x3 of
                                                          {S y4 -> return y4; _ -> mzero};
                                                    x23 <- case x0 of
                                                           {S y23 -> return y23; _ -> mzero};
                                                    let {x6 = x23};
                                                    let {x22 = S x6};
                                                    let {x25 = x6};
                                                    let {x24 = S x25};
                                                    let {x26 = x6};
                                                    x7 <- _addMultiplyIOII x6 x24 x26;
                                                    let {x28 = S x7};
                                                    let {x27 = S x28};
                                                    x5 <- addAddOIII x6 x27 x4;
                                                    x2 <- squareAddAddIIOI x5 x1 x22;
                                                    return x2}]
_addMultiplyIOII x0 x2 x3 = msum [do {guard (x0 == O);
                                      x1 <- multiplyIIO x2 x3;
                                      return x1},
                                  do {x5 <- case x0 of
                                            {S y5 -> return y5; _ -> mzero};
                                      x4 <- _addMultiplyIOII x5 x2 x3;
                                      let {x1 = S x4};
                                      return x1}]
addAddOIII x1 x2 x3 = msum [do {guard (x1 == O);
                                x0 <- addOII x2 x3;
                                return x0},
                            do {x4 <- case x3 of
                                      {S y4 -> return y4; _ -> mzero};
                                x5 <- case x1 of
                                      {S y5 -> return y5; _ -> mzero};
                                x0 <- addAddOIII x5 x2 x4;
                                return x0}]
addOII x1 x2 = msum [do {guard (x1 == O);
                         let {x0 = x2};
                         return x0},
                     do {x3 <- case x2 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- case x1 of
                               {S y4 -> return y4; _ -> mzero};
                         x0 <- addOII x4 x3;
                         return x0}]
multiplyIIO x0 x1 = msum [do {let {x2 = O};
                              guard (x1 == O);
                              return x2},
                          do {x4 <- case x1 of
                                    {S y4 -> return y4; _ -> mzero};
                              let {x18 = x0};
                              x3 <- _addMultiplyIOII x0 x18 x4;
                              let {x2 = S x3};
                              return x2}]
squareII x0 x1 = msum [do {guard (x1 == O);
                           guard (x0 == O);
                           return ()},
                       do {x2 <- case x1 of
                                 {S y2 -> return y2; _ -> mzero};
                           x3 <- case x0 of
                                 {S y3 -> return y3; _ -> mzero};
                           let {x19 = x3};
                           let {x20 = x3};
                           _addMultiplyIIII x3 x2 x19 x20;
                           return ()}]
_addMultiplyIIII x0 x1 x2 x3 = msum [do {multiplyIII x2 x3 x1;
                                         guard (x0 == O);
                                         return ()},
                                     do {x4 <- case x1 of
                                               {S y4 -> return y4; _ -> mzero};
                                         x5 <- case x0 of
                                               {S y5 -> return y5; _ -> mzero};
                                         _addMultiplyIIII x5 x4 x2 x3;
                                         return ()}]
multiplyIII x0 x1 x2 = msum [do {guard (x2 == O);
                                 guard (x1 == O);
                                 return ()},
                             do {x3 <- case x2 of
                                       {S y3 -> return y3; _ -> mzero};
                                 x4 <- case x1 of
                                       {S y4 -> return y4; _ -> mzero};
                                 let {x18 = x0};
                                 _addMultiplyIIII x0 x3 x18 x4;
                                 return ()}]
squareIO x0 = msum [do {let {x1 = O}; guard (x0 == O); return x1},
                    do {x3 <- case x0 of
                              {S y3 -> return y3; _ -> mzero};
                        let {x19 = x3};
                        let {x20 = x3};
                        x2 <- _addMultiplyIOII x3 x19 x20;
                        let {x1 = S x2};
                        return x1}]
squareAddIIO x0 x1 = msum [do {guard (x0 == O);
                               let {x2 = x1};
                               return x2},
                           do {x3 <- case x1 of
                                     {S y3 -> return y3; _ -> mzero};
                               x4 <- case x0 of
                                     {S y4 -> return y4; _ -> mzero};
                               x2 <- addMultiplyAddOII x4 x3;
                               return x2}]
addMultiplyAddOII x1 x2 = msum [do {guard (x1 == O);
                                    let {x0 = x2};
                                    return x0},
                                do {x3 <- case x2 of
                                          {S y3 -> return y3; _ -> mzero};
                                    x15 <- case x1 of
                                           {S y15 -> return y15; _ -> mzero};
                                    let {x4 = x15};
                                    let {x13 = x4};
                                    let {x12 = S x13};
                                    let {x14 = x4};
                                    x5 <- _addMultiplyIOII x4 x12 x14;
                                    let {x17 = S x5};
                                    let {x16 = S x17};
                                    x0 <- addAddOIII x4 x16 x3;
                                    return x0}]
squareAddAddIIOI x0 x1 x3 = msum [do {guard (x3 == O);
                                      x29 <- addOII x1 x0;
                                      x2 <- case x29 of
                                            {S y2 -> return y2; _ -> mzero};
                                      return x2},
                                  do {x4 <- case x3 of
                                            {S y4 -> return y4; _ -> mzero};
                                      x2 <- addMultiplyAddAddIIOI x0 x1 x4;
                                      return x2}]
addMultiplyAddAddIIOI x0 x1 x3 = msum [do {guard (x3 == O);
                                           x30 <- addOII x1 x0;
                                           x31 <- case x30 of
                                                  {S y31 -> return y31; _ -> mzero};
                                           x32 <- case x31 of
                                                  {S y32 -> return y32; _ -> mzero};
                                           x33 <- case x32 of
                                                  {S y33 -> return y33; _ -> mzero};
                                           x2 <- case x33 of
                                                 {S y2 -> return y2; _ -> mzero};
                                           return x2},
                                       do {x4 <- case x3 of
                                                 {S y4 -> return y4; _ -> mzero};
                                           x2 <- addAddAddMultiplyAddAddIIOI x0 x1 x4;
                                           return x2}]
addAddAddMultiplyAddAddIIOI x0 x1 x3 = msum [do {guard (x3 == O);
                                                 x34 <- addOII x1 x0;
                                                 x35 <- case x34 of
                                                        {S y35 -> return y35; _ -> mzero};
                                                 x36 <- case x35 of
                                                        {S y36 -> return y36; _ -> mzero};
                                                 x37 <- case x36 of
                                                        {S y37 -> return y37; _ -> mzero};
                                                 x38 <- case x37 of
                                                        {S y38 -> return y38; _ -> mzero};
                                                 x39 <- case x38 of
                                                        {S y39 -> return y39; _ -> mzero};
                                                 x40 <- case x39 of
                                                        {S y40 -> return y40; _ -> mzero};
                                                 x41 <- case x40 of
                                                        {S y41 -> return y41; _ -> mzero};
                                                 x42 <- case x41 of
                                                        {S y42 -> return y42; _ -> mzero};
                                                 x2 <- case x42 of
                                                       {S y2 -> return y2; _ -> mzero};
                                                 return x2},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 let {x46 = x4};
                                                 let {x45 = S x46};
                                                 let {x44 = S x45};
                                                 let {x43 = S x44};
                                                 let {x47 = x4};
                                                 x5 <- _addMultiplyIOII x4 x43 x47;
                                                 let {x59 = S x5};
                                                 let {x58 = S x59};
                                                 let {x57 = S x58};
                                                 let {x56 = S x57};
                                                 x8 <- addIIO x56 x4;
                                                 let {x55 = S x8};
                                                 let {x54 = S x55};
                                                 let {x53 = S x54};
                                                 let {x52 = S x53};
                                                 x6 <- addIIO x52 x4;
                                                 let {x51 = S x6};
                                                 let {x50 = S x51};
                                                 let {x49 = S x50};
                                                 let {x48 = S x49};
                                                 x60 <- addOII x1 x0;
                                                 x61 <- case x60 of
                                                        {S y61 -> return y61; _ -> mzero};
                                                 x62 <- case x61 of
                                                        {S y62 -> return y62; _ -> mzero};
                                                 x63 <- case x62 of
                                                        {S y63 -> return y63; _ -> mzero};
                                                 x7 <- case x63 of
                                                       {S y7 -> return y7; _ -> mzero};
                                                 x2 <- addAddOIII x4 x48 x7;
                                                 return x2}]
addIIO x0 x1 = msum [do {guard (x1 == O);
                         let {x2 = x0};
                         return x2},
                     do {x4 <- case x1 of
                               {S y4 -> return y4; _ -> mzero};
                         x3 <- addIIO x0 x4;
                         let {x2 = S x3};
                         return x2}]
sumtrsquaretrIIIO x0 x1 x2 = msum [do {let {x10 = O};
                                       guard (x0 == O);
                                       x5 <- squareIO x1;
                                       x4 <- squareAddIOI x10 x5;
                                       x3 <- squareAddIOI x2 x4;
                                       return x3},
                                   do {x11 <- case x0 of
                                              {S y11 -> return y11; _ -> mzero};
                                       let {x7 = x11};
                                       x8 <- squareIO x2;
                                       x9 <- squareIO x1;
                                       x6 <- addMultiplySquareAddAddAddIIIO x7 x8 x9;
                                       let {x3 = S x6};
                                       return x3}]
addMultiplySquareAddAddAddIIIO x0 x1 x2 = msum [do {let {x21 = O};
                                                    guard (x0 == O);
                                                    x3 <- squareAddAddOIII x1 x2 x21;
                                                    return x3},
                                                do {x23 <- case x0 of
                                                           {S y23 -> return y23; _ -> mzero};
                                                    let {x6 = x23};
                                                    let {x22 = S x6};
                                                    let {x25 = x6};
                                                    let {x24 = S x25};
                                                    let {x26 = x6};
                                                    x5 <- squareAddAddOIII x1 x2 x22;
                                                    x7 <- _addMultiplyIOII x6 x24 x26;
                                                    let {x28 = S x7};
                                                    let {x27 = S x28};
                                                    x4 <- addAddIIIO x5 x6 x27;
                                                    let {x3 = S x4};
                                                    return x3}]
addAddIIIO x0 x1 x2 = msum [do {guard (x1 == O);
                                x3 <- addIIO x0 x2;
                                return x3},
                            do {x5 <- case x1 of
                                      {S y5 -> return y5; _ -> mzero};
                                x4 <- addAddIIIO x0 x5 x2;
                                let {x3 = S x4};
                                return x3}]
squareAddIOI x0 x2 = msum [do {guard (x0 == O);
                               let {x1 = x2};
                               return x1},
                           do {x4 <- case x0 of
                                     {S y4 -> return y4; _ -> mzero};
                               x3 <- addMultiplyAddIIO x2 x4;
                               let {x1 = S x3};
                               return x1}]
addMultiplyAddIIO x0 x1 = msum [do {guard (x1 == O);
                                    let {x2 = x0};
                                    return x2},
                                do {x15 <- case x1 of
                                           {S y15 -> return y15; _ -> mzero};
                                    let {x4 = x15};
                                    let {x13 = x4};
                                    let {x12 = S x13};
                                    let {x14 = x4};
                                    x5 <- _addMultiplyIOII x4 x12 x14;
                                    let {x17 = S x5};
                                    let {x16 = S x17};
                                    x3 <- addAddIIIO x0 x4 x16;
                                    let {x2 = S x3};
                                    return x2}]
squareAddAddOIII x1 x2 x3 = msum [do {let {x29 = S x2};
                                      guard (x3 == O);
                                      x0 <- addIIO x29 x1;
                                      return x0},
                                  do {x4 <- case x3 of
                                            {S y4 -> return y4; _ -> mzero};
                                      x0 <- addMultiplyAddAddOIII x1 x2 x4;
                                      return x0}]
addMultiplyAddAddOIII x1 x2 x3 = msum [do {let {x33 = S x2};
                                           let {x32 = S x33};
                                           let {x31 = S x32};
                                           let {x30 = S x31};
                                           guard (x3 == O);
                                           x0 <- addIIO x30 x1;
                                           return x0},
                                       do {x4 <- case x3 of
                                                 {S y4 -> return y4; _ -> mzero};
                                           x0 <- addAddAddMultiplyAddAddOIII x1 x2 x4;
                                           return x0}]
addAddAddMultiplyAddAddOIII x1 x2 x3 = msum [do {let {x42 = S x2};
                                                 let {x41 = S x42};
                                                 let {x40 = S x41};
                                                 let {x39 = S x40};
                                                 let {x38 = S x39};
                                                 let {x37 = S x38};
                                                 let {x36 = S x37};
                                                 let {x35 = S x36};
                                                 let {x34 = S x35};
                                                 guard (x3 == O);
                                                 x0 <- addIIO x34 x1;
                                                 return x0},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 let {x46 = x4};
                                                 let {x45 = S x46};
                                                 let {x44 = S x45};
                                                 let {x43 = S x44};
                                                 let {x47 = x4};
                                                 x5 <- _addMultiplyIOII x4 x43 x47;
                                                 let {x59 = S x5};
                                                 let {x58 = S x59};
                                                 let {x57 = S x58};
                                                 let {x56 = S x57};
                                                 x8 <- addIIO x56 x4;
                                                 let {x55 = S x8};
                                                 let {x54 = S x55};
                                                 let {x53 = S x54};
                                                 let {x52 = S x53};
                                                 x6 <- addIIO x52 x4;
                                                 let {x51 = S x6};
                                                 let {x50 = S x51};
                                                 let {x49 = S x50};
                                                 let {x48 = S x49};
                                                 x7 <- addAddIIIO x2 x4 x48;
                                                 let {x63 = S x7};
                                                 let {x62 = S x63};
                                                 let {x61 = S x62};
                                                 let {x60 = S x61};
                                                 x0 <- addIIO x60 x1;
                                                 return x0}]
sumtrsquaretrIIOI x0 x1 x3 gen_multiplyOOI_x0 = msum [do {let {x10 = O};
                                                          guard (x0 == O);
                                                          x5 <- squareIO x1;
                                                          x4 <- squareAddIOI x10 x5;
                                                          x2 <- squareAddOII x3 x4;
                                                          return x2},
                                                      do {x6 <- case x3 of
                                                                {S y6 -> return y6; _ -> mzero};
                                                          x11 <- case x0 of
                                                                 {S y11 -> return y11; _ -> mzero};
                                                          let {x7 = x11};
                                                          x9 <- squareIO x1;
                                                          x8 <- addMultiplySquareAddAddAddIOII x7 x9 x6;
                                                          x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                          return x2}]
addMultiplySquareAddAddAddIOII x0 x2 x3 = msum [do {let {x21 = O};
                                                    guard (x0 == O);
                                                    x1 <- squareAddAddIOII x3 x2 x21;
                                                    return x1},
                                                do {x4 <- case x3 of
                                                          {S y4 -> return y4; _ -> mzero};
                                                    x23 <- case x0 of
                                                           {S y23 -> return y23; _ -> mzero};
                                                    let {x6 = x23};
                                                    let {x22 = S x6};
                                                    let {x25 = x6};
                                                    let {x24 = S x25};
                                                    let {x26 = x6};
                                                    x7 <- _addMultiplyIOII x6 x24 x26;
                                                    let {x28 = S x7};
                                                    let {x27 = S x28};
                                                    x5 <- addAddOIII x6 x27 x4;
                                                    x1 <- squareAddAddIOII x5 x2 x22;
                                                    return x1}]
squareOI x1 gen_multiplyOOI_x0 = msum [do {let {x0 = O};
                                           guard (x1 == O);
                                           return x0},
                                       do {x2 <- case x1 of
                                                 {S y2 -> return y2; _ -> mzero};
                                           (x3, x19, x20) <- _addMultiplyOIOO x2 gen_multiplyOOI_x0;
                                           guard (x19 == x3);
                                           guard (x20 == x3);
                                           let {x0 = S x3};
                                           return x0}]
_addMultiplyOIOO x1 gen_multiplyOOI_x0 = msum [do {let {x0 = O};
                                                   (x2, x3) <- multiplyOOI x1 gen_multiplyOOI_x0;
                                                   return (x0, x2, x3)},
                                               do {x4 <- case x1 of
                                                         {S y4 -> return y4; _ -> mzero};
                                                   (x5,
                                                    x2,
                                                    x3) <- _addMultiplyOIOO x4 gen_multiplyOOI_x0;
                                                   let {x0 = S x5};
                                                   return (x0, x2, x3)}]
multiplyOOI x2 gen_multiplyOOI_x0 = msum [do {let {x1 = O};
                                              guard (x2 == O);
                                              x0 <- gen_multiplyOOI_x0;
                                              return (x0, x1)},
                                          do {x3 <- case x2 of
                                                    {S y3 -> return y3; _ -> mzero};
                                              (x0,
                                               x18,
                                               x4) <- _addMultiplyOIOO x3 gen_multiplyOOI_x0;
                                              guard (x18 == x0);
                                              let {x1 = S x4};
                                              return (x0, x1)}]
squareAddOII x1 x2 = msum [do {guard (x1 == x2);
                               let {x0 = O};
                               return x0},
                           do {x3 <- case x1 of
                                     {S y3 -> return y3; _ -> mzero};
                               x4 <- addMultiplyAddIOI x2 x3;
                               let {x0 = S x4};
                               return x0}]
addMultiplyAddIOI x0 x2 = msum [do {guard (x0 == x2);
                                    let {x1 = O};
                                    return x1},
                                do {x3 <- case x2 of
                                          {S y3 -> return y3; _ -> mzero};
                                    (x4, x16) <- addAddIOOI x0 x3;
                                    x17 <- case x16 of
                                           {S y17 -> return y17; _ -> mzero};
                                    x5 <- case x17 of
                                          {S y5 -> return y5; _ -> mzero};
                                    let {x13 = x4};
                                    let {x12 = S x13};
                                    let {x14 = x4};
                                    _addMultiplyIIII x4 x5 x12 x14;
                                    let {x15 = x4};
                                    let {x1 = S x15};
                                    return x1}]
addAddIOOI x0 x3 = msum [do {let {x1 = O};
                             x2 <- addIOI x0 x3;
                             return (x1, x2)},
                         do {x4 <- case x3 of
                                   {S y4 -> return y4; _ -> mzero};
                             (x5, x2) <- addAddIOOI x0 x4;
                             let {x1 = S x5};
                             return (x1, x2)}]
addIOI x0 x2 = msum [do {guard (x0 == x2);
                         let {x1 = O};
                         return x1},
                     do {x3 <- case x2 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- addIOI x0 x3;
                         let {x1 = S x4};
                         return x1}]
squareAddAddIOII x0 x2 x3 = msum [do {let {x29 = S x2};
                                      guard (x3 == O);
                                      x1 <- addIOI x29 x0;
                                      return x1},
                                  do {x4 <- case x3 of
                                            {S y4 -> return y4; _ -> mzero};
                                      x1 <- addMultiplyAddAddIOII x0 x2 x4;
                                      return x1}]
addMultiplyAddAddIOII x0 x2 x3 = msum [do {let {x33 = S x2};
                                           let {x32 = S x33};
                                           let {x31 = S x32};
                                           let {x30 = S x31};
                                           guard (x3 == O);
                                           x1 <- addIOI x30 x0;
                                           return x1},
                                       do {x4 <- case x3 of
                                                 {S y4 -> return y4; _ -> mzero};
                                           x1 <- addAddAddMultiplyAddAddIOII x0 x2 x4;
                                           return x1}]
addAddAddMultiplyAddAddIOII x0 x2 x3 = msum [do {let {x42 = S x2};
                                                 let {x41 = S x42};
                                                 let {x40 = S x41};
                                                 let {x39 = S x40};
                                                 let {x38 = S x39};
                                                 let {x37 = S x38};
                                                 let {x36 = S x37};
                                                 let {x35 = S x36};
                                                 let {x34 = S x35};
                                                 guard (x3 == O);
                                                 x1 <- addIOI x34 x0;
                                                 return x1},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 let {x46 = x4};
                                                 let {x45 = S x46};
                                                 let {x44 = S x45};
                                                 let {x43 = S x44};
                                                 let {x47 = x4};
                                                 x5 <- _addMultiplyIOII x4 x43 x47;
                                                 let {x59 = S x5};
                                                 let {x58 = S x59};
                                                 let {x57 = S x58};
                                                 let {x56 = S x57};
                                                 x8 <- addIIO x56 x4;
                                                 let {x55 = S x8};
                                                 let {x54 = S x55};
                                                 let {x53 = S x54};
                                                 let {x52 = S x53};
                                                 x6 <- addIIO x52 x4;
                                                 let {x51 = S x6};
                                                 let {x50 = S x51};
                                                 let {x49 = S x50};
                                                 let {x48 = S x49};
                                                 x7 <- addAddIIIO x2 x4 x48;
                                                 let {x63 = S x7};
                                                 let {x62 = S x63};
                                                 let {x61 = S x62};
                                                 let {x60 = S x61};
                                                 x1 <- addIOI x60 x0;
                                                 return x1}]
sumtrsquaretrIIOO x0 x1 gen_multiplyOOI_x0 = msum [do {let {x10 = O};
                                                       guard (x0 == O);
                                                       x5 <- squareIO x1;
                                                       x4 <- squareAddIOI x10 x5;
                                                       (x2, x3) <- squareAddOOI x4;
                                                       return (x2, x3)},
                                                   do {x11 <- case x0 of
                                                              {S y11 -> return y11; _ -> mzero};
                                                       let {x7 = x11};
                                                       x9 <- squareIO x1;
                                                       (x8,
                                                        x6) <- addMultiplySquareAddAddAddIOIO x7 x9;
                                                       let {x3 = S x6};
                                                       x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                       return (x2, x3)}]
addMultiplySquareAddAddAddIOIO x0 x2 = msum [do {let {x21 = O};
                                                 guard (x0 == O);
                                                 (x3, x1) <- squareAddAddOOII x2 x21;
                                                 return (x1, x3)},
                                             do {x23 <- case x0 of
                                                        {S y23 -> return y23; _ -> mzero};
                                                 let {x6 = x23};
                                                 let {x22 = S x6};
                                                 let {x25 = x6};
                                                 let {x24 = S x25};
                                                 let {x26 = x6};
                                                 x7 <- _addMultiplyIOII x6 x24 x26;
                                                 let {x28 = S x7};
                                                 let {x27 = S x28};
                                                 (x5, x1) <- squareAddAddOOII x2 x22;
                                                 x4 <- addAddIIIO x5 x6 x27;
                                                 let {x3 = S x4};
                                                 return (x1, x3)}]
squareAddOOI x2 = msum [do {let {x0 = O};
                            let {x1 = x2};
                            return (x0, x1)},
                        do {(x4, x3) <- addMultiplyAddIOO x2;
                            let {x1 = S x3};
                            let {x0 = S x4};
                            return (x0, x1)}]
addMultiplyAddIOO x0 = msum [do {let {x1 = O};
                                 let {x2 = x0};
                                 return (x1, x2)},
                             do {(x4, x16, x3) <- addAddIOOO x0;
                                 let {x2 = S x3};
                                 x17 <- case x16 of
                                        {S y17 -> return y17; _ -> mzero};
                                 x5 <- case x17 of
                                       {S y5 -> return y5; _ -> mzero};
                                 let {x13 = x4};
                                 let {x12 = S x13};
                                 let {x14 = x4};
                                 _addMultiplyIIII x4 x5 x12 x14;
                                 let {x15 = x4};
                                 let {x1 = S x15};
                                 return (x1, x2)}]
addAddIOOO x0 = msum [do {let {x1 = O};
                          (x2, x3) <- addIOO x0;
                          return (x1, x2, x3)},
                      do {(x5, x2, x4) <- addAddIOOO x0;
                          let {x3 = S x4};
                          let {x1 = S x5};
                          return (x1, x2, x3)}]
addIOO x0 = msum [do {let {x1 = O};
                      let {x2 = x0};
                      return (x1, x2)},
                  do {(x4, x3) <- addIOO x0;
                      let {x2 = S x3};
                      let {x1 = S x4};
                      return (x1, x2)}]
squareAddAddOOII x2 x3 = msum [do {let {x29 = S x2};
                                   guard (x3 == O);
                                   (x1, x0) <- addIOO x29;
                                   return (x0, x1)},
                               do {x4 <- case x3 of
                                         {S y4 -> return y4; _ -> mzero};
                                   (x0, x1) <- addMultiplyAddAddOOII x2 x4;
                                   return (x0, x1)}]
addMultiplyAddAddOOII x2 x3 = msum [do {let {x33 = S x2};
                                        let {x32 = S x33};
                                        let {x31 = S x32};
                                        let {x30 = S x31};
                                        guard (x3 == O);
                                        (x1, x0) <- addIOO x30;
                                        return (x0, x1)},
                                    do {x4 <- case x3 of
                                              {S y4 -> return y4; _ -> mzero};
                                        (x0, x1) <- addAddAddMultiplyAddAddOOII x2 x4;
                                        return (x0, x1)}]
addAddAddMultiplyAddAddOOII x2 x3 = msum [do {let {x42 = S x2};
                                              let {x41 = S x42};
                                              let {x40 = S x41};
                                              let {x39 = S x40};
                                              let {x38 = S x39};
                                              let {x37 = S x38};
                                              let {x36 = S x37};
                                              let {x35 = S x36};
                                              let {x34 = S x35};
                                              guard (x3 == O);
                                              (x1, x0) <- addIOO x34;
                                              return (x0, x1)},
                                          do {x4 <- case x3 of
                                                    {S y4 -> return y4; _ -> mzero};
                                              let {x46 = x4};
                                              let {x45 = S x46};
                                              let {x44 = S x45};
                                              let {x43 = S x44};
                                              let {x47 = x4};
                                              x5 <- _addMultiplyIOII x4 x43 x47;
                                              let {x59 = S x5};
                                              let {x58 = S x59};
                                              let {x57 = S x58};
                                              let {x56 = S x57};
                                              x8 <- addIIO x56 x4;
                                              let {x55 = S x8};
                                              let {x54 = S x55};
                                              let {x53 = S x54};
                                              let {x52 = S x53};
                                              x6 <- addIIO x52 x4;
                                              let {x51 = S x6};
                                              let {x50 = S x51};
                                              let {x49 = S x50};
                                              let {x48 = S x49};
                                              x7 <- addAddIIIO x2 x4 x48;
                                              let {x63 = S x7};
                                              let {x62 = S x63};
                                              let {x61 = S x62};
                                              let {x60 = S x61};
                                              (x1, x0) <- addIOO x60;
                                              return (x0, x1)}]
sumtrsquaretrIOII x0 x2 x3 gen_multiplyOOI_x0 = msum [do {let {x10 = O};
                                                          guard (x0 == O);
                                                          x4 <- squareAddIIO x2 x3;
                                                          x5 <- squareAddIIO x10 x4;
                                                          x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                          return x1},
                                                      do {x6 <- case x3 of
                                                                {S y6 -> return y6; _ -> mzero};
                                                          x11 <- case x0 of
                                                                 {S y11 -> return y11; _ -> mzero};
                                                          let {x7 = x11};
                                                          x8 <- squareIO x2;
                                                          x9 <- addMultiplySquareAddAddAddIIOI x7 x8 x6;
                                                          x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                          return x1}]
sumtrsquaretrIOIO x0 x2 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 = msum [do {let {x10 = O};
                                                                                                                  guard (x0 == O);
                                                                                                                  (x4,
                                                                                                                   x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                                  x3 <- squareAddIOI x2 x4;
                                                                                                                  x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                                  return (x1,
                                                                                                                          x3)},
                                                                                                              do {x11 <- case x0 of
                                                                                                                         {S y11 -> return y11;
                                                                                                                          _ -> mzero};
                                                                                                                  let {x7 = x11};
                                                                                                                  x8 <- squareIO x2;
                                                                                                                  (x9,
                                                                                                                   x6) <- addMultiplySquareAddAddAddIIOO x7 x8 gen_addOIO_x2;
                                                                                                                  let {x3 = S x6};
                                                                                                                  x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                                  return (x1,
                                                                                                                          x3)}]
addMultiplySquareAddAddAddIIOO x0 x1 gen_addOIO_x2 = msum [do {let {x21 = O};
                                                               guard (x0 == O);
                                                               (x3,
                                                                x2) <- squareAddAddOIOI x1 x21 gen_addOIO_x2;
                                                               return (x2, x3)},
                                                           do {x23 <- case x0 of
                                                                      {S y23 -> return y23;
                                                                       _ -> mzero};
                                                               let {x6 = x23};
                                                               let {x22 = S x6};
                                                               let {x25 = x6};
                                                               let {x24 = S x25};
                                                               let {x26 = x6};
                                                               x7 <- _addMultiplyIOII x6 x24 x26;
                                                               let {x28 = S x7};
                                                               let {x27 = S x28};
                                                               (x5,
                                                                x2) <- squareAddAddOIOI x1 x22 gen_addOIO_x2;
                                                               x4 <- addAddIIIO x5 x6 x27;
                                                               let {x3 = S x4};
                                                               return (x2, x3)}]
squareAddIOO x0 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2 = msum [do {guard (x0 == O);
                                                                                       (x1,
                                                                                        x2) <- do {x2 <- gen_squareAddIOO_x2;
                                                                                                   return (x2,
                                                                                                           x2)};
                                                                                       return (x1,
                                                                                               x2)},
                                                                                   do {x4 <- case x0 of
                                                                                             {S y4 -> return y4;
                                                                                              _ -> mzero};
                                                                                       (x2,
                                                                                        x3) <- addMultiplyAddOIO x4 gen_addMultiplyAddOIO_x2 gen_addOIO_x2;
                                                                                       let {x1 = S x3};
                                                                                       return (x1,
                                                                                               x2)}]
addMultiplyAddOIO x1 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 = msum [do {guard (x1 == O);
                                                                        (x0,
                                                                         x2) <- do {x2 <- gen_addMultiplyAddOIO_x2;
                                                                                    return (x2,
                                                                                            x2)};
                                                                        return (x0, x2)},
                                                                    do {x15 <- case x1 of
                                                                               {S y15 -> return y15;
                                                                                _ -> mzero};
                                                                        let {x4 = x15};
                                                                        let {x13 = x4};
                                                                        let {x12 = S x13};
                                                                        let {x14 = x4};
                                                                        x5 <- _addMultiplyIOII x4 x12 x14;
                                                                        let {x17 = S x5};
                                                                        let {x16 = S x17};
                                                                        (x0,
                                                                         x3) <- addAddOIIO x4 x16 gen_addOIO_x2;
                                                                        let {x2 = S x3};
                                                                        return (x0, x2)}]
addAddOIIO x1 x2 gen_addOIO_x2 = msum [do {guard (x1 == O);
                                           (x0, x3) <- addOIO x2 gen_addOIO_x2;
                                           return (x0, x3)},
                                       do {x5 <- case x1 of
                                                 {S y5 -> return y5; _ -> mzero};
                                           (x0, x4) <- addAddOIIO x5 x2 gen_addOIO_x2;
                                           let {x3 = S x4};
                                           return (x0, x3)}]
addOIO x1 gen_addOIO_x2 = msum [do {guard (x1 == O);
                                    (x0, x2) <- do {x2 <- gen_addOIO_x2; return (x2, x2)};
                                    return (x0, x2)},
                                do {x4 <- case x1 of
                                          {S y4 -> return y4; _ -> mzero};
                                    (x0, x3) <- addOIO x4 gen_addOIO_x2;
                                    let {x2 = S x3};
                                    return (x0, x2)}]
squareAddAddOIOI x1 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                 (x29, x0) <- addOIO x1 gen_addOIO_x2;
                                                 x2 <- case x29 of
                                                       {S y2 -> return y2; _ -> mzero};
                                                 return (x0, x2)},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 (x0,
                                                  x2) <- addMultiplyAddAddOIOI x1 x4 gen_addOIO_x2;
                                                 return (x0, x2)}]
addMultiplyAddAddOIOI x1 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                      (x30, x0) <- addOIO x1 gen_addOIO_x2;
                                                      x31 <- case x30 of
                                                             {S y31 -> return y31; _ -> mzero};
                                                      x32 <- case x31 of
                                                             {S y32 -> return y32; _ -> mzero};
                                                      x33 <- case x32 of
                                                             {S y33 -> return y33; _ -> mzero};
                                                      x2 <- case x33 of
                                                            {S y2 -> return y2; _ -> mzero};
                                                      return (x0, x2)},
                                                  do {x4 <- case x3 of
                                                            {S y4 -> return y4; _ -> mzero};
                                                      (x0,
                                                       x2) <- addAddAddMultiplyAddAddOIOI x1 x4 gen_addOIO_x2;
                                                      return (x0, x2)}]
addAddAddMultiplyAddAddOIOI x1 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                            (x34, x0) <- addOIO x1 gen_addOIO_x2;
                                                            x35 <- case x34 of
                                                                   {S y35 -> return y35;
                                                                    _ -> mzero};
                                                            x36 <- case x35 of
                                                                   {S y36 -> return y36;
                                                                    _ -> mzero};
                                                            x37 <- case x36 of
                                                                   {S y37 -> return y37;
                                                                    _ -> mzero};
                                                            x38 <- case x37 of
                                                                   {S y38 -> return y38;
                                                                    _ -> mzero};
                                                            x39 <- case x38 of
                                                                   {S y39 -> return y39;
                                                                    _ -> mzero};
                                                            x40 <- case x39 of
                                                                   {S y40 -> return y40;
                                                                    _ -> mzero};
                                                            x41 <- case x40 of
                                                                   {S y41 -> return y41;
                                                                    _ -> mzero};
                                                            x42 <- case x41 of
                                                                   {S y42 -> return y42;
                                                                    _ -> mzero};
                                                            x2 <- case x42 of
                                                                  {S y2 -> return y2; _ -> mzero};
                                                            return (x0, x2)},
                                                        do {x4 <- case x3 of
                                                                  {S y4 -> return y4; _ -> mzero};
                                                            let {x46 = x4};
                                                            let {x45 = S x46};
                                                            let {x44 = S x45};
                                                            let {x43 = S x44};
                                                            let {x47 = x4};
                                                            x5 <- _addMultiplyIOII x4 x43 x47;
                                                            let {x59 = S x5};
                                                            let {x58 = S x59};
                                                            let {x57 = S x58};
                                                            let {x56 = S x57};
                                                            x8 <- addIIO x56 x4;
                                                            let {x55 = S x8};
                                                            let {x54 = S x55};
                                                            let {x53 = S x54};
                                                            let {x52 = S x53};
                                                            x6 <- addIIO x52 x4;
                                                            let {x51 = S x6};
                                                            let {x50 = S x51};
                                                            let {x49 = S x50};
                                                            let {x48 = S x49};
                                                            (x2,
                                                             x7) <- addAddOIIO x4 x48 gen_addOIO_x2;
                                                            let {x63 = S x7};
                                                            let {x62 = S x63};
                                                            let {x61 = S x62};
                                                            let {x60 = S x61};
                                                            x0 <- addIIO x60 x1;
                                                            return (x0, x2)}]
sumtrsquaretrIOOI x0 x3 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 = msum [do {let {x10 = O};
                                                                                                                  guard (x0 == O);
                                                                                                                  (x4,
                                                                                                                   x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                                  x2 <- squareAddOII x3 x4;
                                                                                                                  x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                                  return (x1,
                                                                                                                          x2)},
                                                                                                              do {x6 <- case x3 of
                                                                                                                        {S y6 -> return y6;
                                                                                                                         _ -> mzero};
                                                                                                                  x11 <- case x0 of
                                                                                                                         {S y11 -> return y11;
                                                                                                                          _ -> mzero};
                                                                                                                  let {x7 = x11};
                                                                                                                  (x8,
                                                                                                                   x9) <- addMultiplySquareAddAddAddIOOI x7 x6 gen_addOIO_x2;
                                                                                                                  x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                                                                                  x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                                  return (x1,
                                                                                                                          x2)}]
addMultiplySquareAddAddAddIOOI x0 x3 gen_addOIO_x2 = msum [do {let {x21 = O};
                                                               guard (x0 == O);
                                                               (x1,
                                                                x2) <- squareAddAddIOOI x3 x21 gen_addOIO_x2;
                                                               return (x1, x2)},
                                                           do {x4 <- case x3 of
                                                                     {S y4 -> return y4;
                                                                      _ -> mzero};
                                                               x23 <- case x0 of
                                                                      {S y23 -> return y23;
                                                                       _ -> mzero};
                                                               let {x6 = x23};
                                                               let {x22 = S x6};
                                                               let {x25 = x6};
                                                               let {x24 = S x25};
                                                               let {x26 = x6};
                                                               x7 <- _addMultiplyIOII x6 x24 x26;
                                                               let {x28 = S x7};
                                                               let {x27 = S x28};
                                                               x5 <- addAddOIII x6 x27 x4;
                                                               (x1,
                                                                x2) <- squareAddAddIOOI x5 x22 gen_addOIO_x2;
                                                               return (x1, x2)}]
squareAddAddIOOI x0 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                 (x29, x1) <- addOOI x0;
                                                 x2 <- case x29 of
                                                       {S y2 -> return y2; _ -> mzero};
                                                 return (x1, x2)},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 (x1,
                                                  x2) <- addMultiplyAddAddIOOI x0 x4 gen_addOIO_x2;
                                                 return (x1, x2)}]
addOOI x2 = msum [do {let {x1 = O};
                      let {x0 = x2};
                      return (x0, x1)},
                  do {x3 <- case x2 of
                            {S y3 -> return y3; _ -> mzero};
                      (x0, x4) <- addOOI x3;
                      let {x1 = S x4};
                      return (x0, x1)}]
addMultiplyAddAddIOOI x0 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                      (x30, x1) <- addOOI x0;
                                                      x31 <- case x30 of
                                                             {S y31 -> return y31; _ -> mzero};
                                                      x32 <- case x31 of
                                                             {S y32 -> return y32; _ -> mzero};
                                                      x33 <- case x32 of
                                                             {S y33 -> return y33; _ -> mzero};
                                                      x2 <- case x33 of
                                                            {S y2 -> return y2; _ -> mzero};
                                                      return (x1, x2)},
                                                  do {x4 <- case x3 of
                                                            {S y4 -> return y4; _ -> mzero};
                                                      (x1,
                                                       x2) <- addAddAddMultiplyAddAddIOOI x0 x4 gen_addOIO_x2;
                                                      return (x1, x2)}]
addAddAddMultiplyAddAddIOOI x0 x3 gen_addOIO_x2 = msum [do {guard (x3 == O);
                                                            (x34, x1) <- addOOI x0;
                                                            x35 <- case x34 of
                                                                   {S y35 -> return y35;
                                                                    _ -> mzero};
                                                            x36 <- case x35 of
                                                                   {S y36 -> return y36;
                                                                    _ -> mzero};
                                                            x37 <- case x36 of
                                                                   {S y37 -> return y37;
                                                                    _ -> mzero};
                                                            x38 <- case x37 of
                                                                   {S y38 -> return y38;
                                                                    _ -> mzero};
                                                            x39 <- case x38 of
                                                                   {S y39 -> return y39;
                                                                    _ -> mzero};
                                                            x40 <- case x39 of
                                                                   {S y40 -> return y40;
                                                                    _ -> mzero};
                                                            x41 <- case x40 of
                                                                   {S y41 -> return y41;
                                                                    _ -> mzero};
                                                            x42 <- case x41 of
                                                                   {S y42 -> return y42;
                                                                    _ -> mzero};
                                                            x2 <- case x42 of
                                                                  {S y2 -> return y2; _ -> mzero};
                                                            return (x1, x2)},
                                                        do {x4 <- case x3 of
                                                                  {S y4 -> return y4; _ -> mzero};
                                                            let {x46 = x4};
                                                            let {x45 = S x46};
                                                            let {x44 = S x45};
                                                            let {x43 = S x44};
                                                            let {x47 = x4};
                                                            x5 <- _addMultiplyIOII x4 x43 x47;
                                                            let {x59 = S x5};
                                                            let {x58 = S x59};
                                                            let {x57 = S x58};
                                                            let {x56 = S x57};
                                                            x8 <- addIIO x56 x4;
                                                            let {x55 = S x8};
                                                            let {x54 = S x55};
                                                            let {x53 = S x54};
                                                            let {x52 = S x53};
                                                            x6 <- addIIO x52 x4;
                                                            let {x51 = S x6};
                                                            let {x50 = S x51};
                                                            let {x49 = S x50};
                                                            let {x48 = S x49};
                                                            (x2,
                                                             x7) <- addAddOIIO x4 x48 gen_addOIO_x2;
                                                            let {x63 = S x7};
                                                            let {x62 = S x63};
                                                            let {x61 = S x62};
                                                            let {x60 = S x61};
                                                            x1 <- addIOI x60 x0;
                                                            return (x1, x2)}]
sumtrsquaretrIOOO x0 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_addOOO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 = msum [do {let {x10 = O};
                                                                                                                             guard (x0 == O);
                                                                                                                             (x4,
                                                                                                                              x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                                             x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                                             (x2,
                                                                                                                              x3) <- squareAddOOI x4;
                                                                                                                             return (x1,
                                                                                                                                     x2,
                                                                                                                                     x3)},
                                                                                                                         do {x11 <- case x0 of
                                                                                                                                    {S y11 -> return y11;
                                                                                                                                     _ -> mzero};
                                                                                                                             let {x7 = x11};
                                                                                                                             (x8,
                                                                                                                              x9,
                                                                                                                              x6) <- addMultiplySquareAddAddAddIOOO x7 gen_addOIO_x2 gen_addOOO_x2;
                                                                                                                             let {x3 = S x6};
                                                                                                                             x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                                                                                             x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                                             return (x1,
                                                                                                                                     x2,
                                                                                                                                     x3)}]
addMultiplySquareAddAddAddIOOO x0 gen_addOIO_x2 gen_addOOO_x2 = msum [do {let {x21 = O};
                                                                          guard (x0 == O);
                                                                          (x3,
                                                                           x1,
                                                                           x2) <- squareAddAddOOOI x21 gen_addOIO_x2 gen_addOOO_x2;
                                                                          return (x1, x2, x3)},
                                                                      do {x23 <- case x0 of
                                                                                 {S y23 -> return y23;
                                                                                  _ -> mzero};
                                                                          let {x6 = x23};
                                                                          let {x22 = S x6};
                                                                          let {x25 = x6};
                                                                          let {x24 = S x25};
                                                                          let {x26 = x6};
                                                                          x7 <- _addMultiplyIOII x6 x24 x26;
                                                                          let {x28 = S x7};
                                                                          let {x27 = S x28};
                                                                          (x5,
                                                                           x1,
                                                                           x2) <- squareAddAddOOOI x22 gen_addOIO_x2 gen_addOOO_x2;
                                                                          x4 <- addAddIIIO x5 x6 x27;
                                                                          let {x3 = S x4};
                                                                          return (x1, x2, x3)}]
squareAddAddOOOI x3 gen_addOIO_x2 gen_addOOO_x2 = msum [do {guard (x3 == O);
                                                            (x29, x1, x0) <- addOOO gen_addOOO_x2;
                                                            x2 <- case x29 of
                                                                  {S y2 -> return y2; _ -> mzero};
                                                            return (x0, x1, x2)},
                                                        do {x4 <- case x3 of
                                                                  {S y4 -> return y4; _ -> mzero};
                                                            (x0,
                                                             x1,
                                                             x2) <- addMultiplyAddAddOOOI x4 gen_addOIO_x2 gen_addOOO_x2;
                                                            return (x0, x1, x2)}]
addOOO gen_addOOO_x2 = msum [do {let {x1 = O};
                                 (x0, x2) <- do {x2 <- gen_addOOO_x2; return (x2, x2)};
                                 return (x0, x1, x2)},
                             do {(x0, x4, x3) <- addOOO gen_addOOO_x2;
                                 let {x2 = S x3};
                                 let {x1 = S x4};
                                 return (x0, x1, x2)}]
addMultiplyAddAddOOOI x3 gen_addOIO_x2 gen_addOOO_x2 = msum [do {guard (x3 == O);
                                                                 (x30,
                                                                  x1,
                                                                  x0) <- addOOO gen_addOOO_x2;
                                                                 x31 <- case x30 of
                                                                        {S y31 -> return y31;
                                                                         _ -> mzero};
                                                                 x32 <- case x31 of
                                                                        {S y32 -> return y32;
                                                                         _ -> mzero};
                                                                 x33 <- case x32 of
                                                                        {S y33 -> return y33;
                                                                         _ -> mzero};
                                                                 x2 <- case x33 of
                                                                       {S y2 -> return y2;
                                                                        _ -> mzero};
                                                                 return (x0, x1, x2)},
                                                             do {x4 <- case x3 of
                                                                       {S y4 -> return y4;
                                                                        _ -> mzero};
                                                                 (x0,
                                                                  x1,
                                                                  x2) <- addAddAddMultiplyAddAddOOOI x4 gen_addOIO_x2 gen_addOOO_x2;
                                                                 return (x0, x1, x2)}]
addAddAddMultiplyAddAddOOOI x3 gen_addOIO_x2 gen_addOOO_x2 = msum [do {guard (x3 == O);
                                                                       (x34,
                                                                        x1,
                                                                        x0) <- addOOO gen_addOOO_x2;
                                                                       x35 <- case x34 of
                                                                              {S y35 -> return y35;
                                                                               _ -> mzero};
                                                                       x36 <- case x35 of
                                                                              {S y36 -> return y36;
                                                                               _ -> mzero};
                                                                       x37 <- case x36 of
                                                                              {S y37 -> return y37;
                                                                               _ -> mzero};
                                                                       x38 <- case x37 of
                                                                              {S y38 -> return y38;
                                                                               _ -> mzero};
                                                                       x39 <- case x38 of
                                                                              {S y39 -> return y39;
                                                                               _ -> mzero};
                                                                       x40 <- case x39 of
                                                                              {S y40 -> return y40;
                                                                               _ -> mzero};
                                                                       x41 <- case x40 of
                                                                              {S y41 -> return y41;
                                                                               _ -> mzero};
                                                                       x42 <- case x41 of
                                                                              {S y42 -> return y42;
                                                                               _ -> mzero};
                                                                       x2 <- case x42 of
                                                                             {S y2 -> return y2;
                                                                              _ -> mzero};
                                                                       return (x0, x1, x2)},
                                                                   do {x4 <- case x3 of
                                                                             {S y4 -> return y4;
                                                                              _ -> mzero};
                                                                       let {x46 = x4};
                                                                       let {x45 = S x46};
                                                                       let {x44 = S x45};
                                                                       let {x43 = S x44};
                                                                       let {x47 = x4};
                                                                       x5 <- _addMultiplyIOII x4 x43 x47;
                                                                       let {x59 = S x5};
                                                                       let {x58 = S x59};
                                                                       let {x57 = S x58};
                                                                       let {x56 = S x57};
                                                                       x8 <- addIIO x56 x4;
                                                                       let {x55 = S x8};
                                                                       let {x54 = S x55};
                                                                       let {x53 = S x54};
                                                                       let {x52 = S x53};
                                                                       x6 <- addIIO x52 x4;
                                                                       let {x51 = S x6};
                                                                       let {x50 = S x51};
                                                                       let {x49 = S x50};
                                                                       let {x48 = S x49};
                                                                       (x2,
                                                                        x7) <- addAddOIIO x4 x48 gen_addOIO_x2;
                                                                       let {x63 = S x7};
                                                                       let {x62 = S x63};
                                                                       let {x61 = S x62};
                                                                       let {x60 = S x61};
                                                                       (x1, x0) <- addIOO x60;
                                                                       return (x0, x1, x2)}]
sumtrsquaretrOIII x1 x2 x3 = msum [do {let {x0 = O};
                                       let {x10 = O};
                                       x4 <- squareAddIIO x2 x3;
                                       x5 <- squareAddIIO x10 x4;
                                       squareII x1 x5;
                                       return x0},
                                   do {x6 <- case x3 of
                                             {S y6 -> return y6; _ -> mzero};
                                       x8 <- squareIO x2;
                                       x9 <- squareIO x1;
                                       x7 <- addMultiplySquareAddAddAddOIII x8 x9 x6;
                                       let {x11 = x7};
                                       let {x0 = S x11};
                                       return x0}]
addMultiplySquareAddAddAddOIII x1 x2 x3 = msum [do {let {x0 = O};
                                                    let {x21 = O};
                                                    squareAddAddIIII x3 x1 x2 x21;
                                                    return x0},
                                                do {x4 <- case x3 of
                                                          {S y4 -> return y4; _ -> mzero};
                                                    (x5, x22) <- squareAddAddOIIO x1 x2;
                                                    x6 <- case x22 of
                                                          {S y6 -> return y6; _ -> mzero};
                                                    let {x23 = x6};
                                                    let {x0 = S x23};
                                                    let {x25 = x6};
                                                    let {x24 = S x25};
                                                    let {x26 = x6};
                                                    x7 <- _addMultiplyIOII x6 x24 x26;
                                                    let {x28 = S x7};
                                                    let {x27 = S x28};
                                                    addAddIIII x5 x6 x27 x4;
                                                    return x0}]
addAddIIII x0 x1 x2 x3 = msum [do {addIII x0 x2 x3;
                                   guard (x1 == O);
                                   return ()},
                               do {x4 <- case x3 of
                                         {S y4 -> return y4; _ -> mzero};
                                   x5 <- case x1 of
                                         {S y5 -> return y5; _ -> mzero};
                                   addAddIIII x0 x5 x2 x4;
                                   return ()}]
addIII x0 x1 x2 = msum [do {guard (x0 == x2);
                            guard (x1 == O);
                            return ()},
                        do {x3 <- case x2 of
                                  {S y3 -> return y3; _ -> mzero};
                            x4 <- case x1 of
                                  {S y4 -> return y4; _ -> mzero};
                            addIII x0 x4 x3;
                            return ()}]
squareAddAddIIII x0 x1 x2 x3 = msum [do {let {x29 = S x2};
                                         addIII x29 x1 x0;
                                         guard (x3 == O);
                                         return ()},
                                     do {x4 <- case x3 of
                                               {S y4 -> return y4; _ -> mzero};
                                         addMultiplyAddAddIIII x0 x1 x2 x4;
                                         return ()}]
addMultiplyAddAddIIII x0 x1 x2 x3 = msum [do {let {x33 = S x2};
                                              let {x32 = S x33};
                                              let {x31 = S x32};
                                              let {x30 = S x31};
                                              addIII x30 x1 x0;
                                              guard (x3 == O);
                                              return ()},
                                          do {x4 <- case x3 of
                                                    {S y4 -> return y4; _ -> mzero};
                                              addAddAddMultiplyAddAddIIII x0 x1 x2 x4;
                                              return ()}]
addAddAddMultiplyAddAddIIII x0 x1 x2 x3 = msum [do {let {x42 = S x2};
                                                    let {x41 = S x42};
                                                    let {x40 = S x41};
                                                    let {x39 = S x40};
                                                    let {x38 = S x39};
                                                    let {x37 = S x38};
                                                    let {x36 = S x37};
                                                    let {x35 = S x36};
                                                    let {x34 = S x35};
                                                    addIII x34 x1 x0;
                                                    guard (x3 == O);
                                                    return ()},
                                                do {x4 <- case x3 of
                                                          {S y4 -> return y4; _ -> mzero};
                                                    let {x46 = x4};
                                                    let {x45 = S x46};
                                                    let {x44 = S x45};
                                                    let {x43 = S x44};
                                                    let {x47 = x4};
                                                    x5 <- _addMultiplyIOII x4 x43 x47;
                                                    let {x59 = S x5};
                                                    let {x58 = S x59};
                                                    let {x57 = S x58};
                                                    let {x56 = S x57};
                                                    x8 <- addIIO x56 x4;
                                                    let {x55 = S x8};
                                                    let {x54 = S x55};
                                                    let {x53 = S x54};
                                                    let {x52 = S x53};
                                                    x6 <- addIIO x52 x4;
                                                    let {x51 = S x6};
                                                    let {x50 = S x51};
                                                    let {x49 = S x50};
                                                    let {x48 = S x49};
                                                    x7 <- addAddIIIO x2 x4 x48;
                                                    let {x63 = S x7};
                                                    let {x62 = S x63};
                                                    let {x61 = S x62};
                                                    let {x60 = S x61};
                                                    addIII x60 x1 x0;
                                                    return ()}]
squareAddAddOIIO x1 x2 = msum [do {let {x3 = O};
                                   let {x29 = S x2};
                                   x0 <- addIIO x29 x1;
                                   return (x0, x3)},
                               do {(x0, x4) <- addMultiplyAddAddOIIO x1 x2;
                                   let {x3 = S x4};
                                   return (x0, x3)}]
addMultiplyAddAddOIIO x1 x2 = msum [do {let {x3 = O};
                                        let {x33 = S x2};
                                        let {x32 = S x33};
                                        let {x31 = S x32};
                                        let {x30 = S x31};
                                        x0 <- addIIO x30 x1;
                                        return (x0, x3)},
                                    do {(x0, x4) <- addAddAddMultiplyAddAddOIIO x1 x2;
                                        let {x3 = S x4};
                                        return (x0, x3)}]
addAddAddMultiplyAddAddOIIO x1 x2 = msum [do {let {x3 = O};
                                              let {x42 = S x2};
                                              let {x41 = S x42};
                                              let {x40 = S x41};
                                              let {x39 = S x40};
                                              let {x38 = S x39};
                                              let {x37 = S x38};
                                              let {x36 = S x37};
                                              let {x35 = S x36};
                                              let {x34 = S x35};
                                              x0 <- addIIO x34 x1;
                                              return (x0, x3)},
                                          do {(x4, x48, x7) <- addAddIOOO x2;
                                              let {x3 = S x4};
                                              let {x63 = S x7};
                                              let {x62 = S x63};
                                              let {x61 = S x62};
                                              let {x60 = S x61};
                                              x49 <- case x48 of
                                                     {S y49 -> return y49; _ -> mzero};
                                              x50 <- case x49 of
                                                     {S y50 -> return y50; _ -> mzero};
                                              x51 <- case x50 of
                                                     {S y51 -> return y51; _ -> mzero};
                                              x6 <- case x51 of
                                                    {S y6 -> return y6; _ -> mzero};
                                              let {x46 = x4};
                                              let {x45 = S x46};
                                              let {x44 = S x45};
                                              let {x43 = S x44};
                                              let {x47 = x4};
                                              x5 <- _addMultiplyIOII x4 x43 x47;
                                              let {x59 = S x5};
                                              let {x58 = S x59};
                                              let {x57 = S x58};
                                              let {x56 = S x57};
                                              x52 <- addOII x4 x6;
                                              x53 <- case x52 of
                                                     {S y53 -> return y53; _ -> mzero};
                                              x54 <- case x53 of
                                                     {S y54 -> return y54; _ -> mzero};
                                              x55 <- case x54 of
                                                     {S y55 -> return y55; _ -> mzero};
                                              x8 <- case x55 of
                                                    {S y8 -> return y8; _ -> mzero};
                                              addIII x56 x4 x8;
                                              x0 <- addIIO x60 x1;
                                              return (x0, x3)}]
sumtrsquaretrOIIO x1 x2 = msum [do {let {x0 = O};
                                    let {x10 = O};
                                    x5 <- squareIO x1;
                                    x4 <- squareAddIOI x10 x5;
                                    x3 <- squareAddIOI x2 x4;
                                    return (x0, x3)},
                                do {x8 <- squareIO x2;
                                    x9 <- squareIO x1;
                                    (x7, x6) <- addMultiplySquareAddAddAddOIIO x8 x9;
                                    let {x3 = S x6};
                                    let {x11 = x7};
                                    let {x0 = S x11};
                                    return (x0, x3)}]
addMultiplySquareAddAddAddOIIO x1 x2 = msum [do {let {x0 = O};
                                                 let {x21 = O};
                                                 x3 <- squareAddAddOIII x1 x2 x21;
                                                 return (x0, x3)},
                                             do {(x5, x22) <- squareAddAddOIIO x1 x2;
                                                 x6 <- case x22 of
                                                       {S y6 -> return y6; _ -> mzero};
                                                 let {x23 = x6};
                                                 let {x0 = S x23};
                                                 let {x25 = x6};
                                                 let {x24 = S x25};
                                                 let {x26 = x6};
                                                 x7 <- _addMultiplyIOII x6 x24 x26;
                                                 let {x28 = S x7};
                                                 let {x27 = S x28};
                                                 x4 <- addAddIIIO x5 x6 x27;
                                                 let {x3 = S x4};
                                                 return (x0, x3)}]
sumtrsquaretrOIOI x1 x3 gen_multiplyOOI_x0 = msum [do {let {x0 = O};
                                                       let {x10 = O};
                                                       x5 <- squareIO x1;
                                                       x4 <- squareAddIOI x10 x5;
                                                       x2 <- squareAddOII x3 x4;
                                                       return (x0, x2)},
                                                   do {x6 <- case x3 of
                                                             {S y6 -> return y6; _ -> mzero};
                                                       x9 <- squareIO x1;
                                                       (x7,
                                                        x8) <- addMultiplySquareAddAddAddOOII x9 x6;
                                                       let {x11 = x7};
                                                       let {x0 = S x11};
                                                       x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                       return (x0, x2)}]
addMultiplySquareAddAddAddOOII x2 x3 = msum [do {let {x0 = O};
                                                 let {x21 = O};
                                                 x1 <- squareAddAddIOII x3 x2 x21;
                                                 return (x0, x1)},
                                             do {x4 <- case x3 of
                                                       {S y4 -> return y4; _ -> mzero};
                                                 (x5, x1, x22) <- squareAddAddOOIO x2;
                                                 x6 <- case x22 of
                                                       {S y6 -> return y6; _ -> mzero};
                                                 let {x23 = x6};
                                                 let {x0 = S x23};
                                                 let {x25 = x6};
                                                 let {x24 = S x25};
                                                 let {x26 = x6};
                                                 x7 <- _addMultiplyIOII x6 x24 x26;
                                                 let {x28 = S x7};
                                                 let {x27 = S x28};
                                                 addAddIIII x5 x6 x27 x4;
                                                 return (x0, x1)}]
squareAddAddOOIO x2 = msum [do {let {x3 = O};
                                let {x29 = S x2};
                                (x1, x0) <- addIOO x29;
                                return (x0, x1, x3)},
                            do {(x0, x1, x4) <- addMultiplyAddAddOOIO x2;
                                let {x3 = S x4};
                                return (x0, x1, x3)}]
addMultiplyAddAddOOIO x2 = msum [do {let {x3 = O};
                                     let {x33 = S x2};
                                     let {x32 = S x33};
                                     let {x31 = S x32};
                                     let {x30 = S x31};
                                     (x1, x0) <- addIOO x30;
                                     return (x0, x1, x3)},
                                 do {(x0, x1, x4) <- addAddAddMultiplyAddAddOOIO x2;
                                     let {x3 = S x4};
                                     return (x0, x1, x3)}]
addAddAddMultiplyAddAddOOIO x2 = msum [do {let {x3 = O};
                                           let {x42 = S x2};
                                           let {x41 = S x42};
                                           let {x40 = S x41};
                                           let {x39 = S x40};
                                           let {x38 = S x39};
                                           let {x37 = S x38};
                                           let {x36 = S x37};
                                           let {x35 = S x36};
                                           let {x34 = S x35};
                                           (x1, x0) <- addIOO x34;
                                           return (x0, x1, x3)},
                                       do {(x4, x48, x7) <- addAddIOOO x2;
                                           let {x3 = S x4};
                                           let {x63 = S x7};
                                           let {x62 = S x63};
                                           let {x61 = S x62};
                                           let {x60 = S x61};
                                           x49 <- case x48 of
                                                  {S y49 -> return y49; _ -> mzero};
                                           x50 <- case x49 of
                                                  {S y50 -> return y50; _ -> mzero};
                                           x51 <- case x50 of
                                                  {S y51 -> return y51; _ -> mzero};
                                           x6 <- case x51 of
                                                 {S y6 -> return y6; _ -> mzero};
                                           let {x46 = x4};
                                           let {x45 = S x46};
                                           let {x44 = S x45};
                                           let {x43 = S x44};
                                           let {x47 = x4};
                                           x5 <- _addMultiplyIOII x4 x43 x47;
                                           let {x59 = S x5};
                                           let {x58 = S x59};
                                           let {x57 = S x58};
                                           let {x56 = S x57};
                                           x52 <- addOII x4 x6;
                                           x53 <- case x52 of
                                                  {S y53 -> return y53; _ -> mzero};
                                           x54 <- case x53 of
                                                  {S y54 -> return y54; _ -> mzero};
                                           x55 <- case x54 of
                                                  {S y55 -> return y55; _ -> mzero};
                                           x8 <- case x55 of
                                                 {S y8 -> return y8; _ -> mzero};
                                           addIII x56 x4 x8;
                                           (x1, x0) <- addIOO x60;
                                           return (x0, x1, x3)}]
sumtrsquaretrOIOO x1 gen_multiplyOOI_x0 = msum [do {let {x0 = O};
                                                    let {x10 = O};
                                                    x5 <- squareIO x1;
                                                    x4 <- squareAddIOI x10 x5;
                                                    (x2, x3) <- squareAddOOI x4;
                                                    return (x0, x2, x3)},
                                                do {x9 <- squareIO x1;
                                                    (x7,
                                                     x8,
                                                     x6) <- addMultiplySquareAddAddAddOOIO x9;
                                                    let {x3 = S x6};
                                                    let {x11 = x7};
                                                    let {x0 = S x11};
                                                    x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                    return (x0, x2, x3)}]
addMultiplySquareAddAddAddOOIO x2 = msum [do {let {x0 = O};
                                              let {x21 = O};
                                              (x3, x1) <- squareAddAddOOII x2 x21;
                                              return (x0, x1, x3)},
                                          do {(x5, x1, x22) <- squareAddAddOOIO x2;
                                              x6 <- case x22 of
                                                    {S y6 -> return y6; _ -> mzero};
                                              let {x23 = x6};
                                              let {x0 = S x23};
                                              let {x25 = x6};
                                              let {x24 = S x25};
                                              let {x26 = x6};
                                              x7 <- _addMultiplyIOII x6 x24 x26;
                                              let {x28 = S x7};
                                              let {x27 = S x28};
                                              x4 <- addAddIIIO x5 x6 x27;
                                              let {x3 = S x4};
                                              return (x0, x1, x3)}]
sumtrsquaretrOOII x2 x3 gen_addOIO_x2 gen_multiplyOOI_x0 = msum [do {let {x0 = O};
                                                                     let {x10 = O};
                                                                     x4 <- squareAddIIO x2 x3;
                                                                     x5 <- squareAddIIO x10 x4;
                                                                     x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                     return (x0, x1)},
                                                                 do {x6 <- case x3 of
                                                                           {S y6 -> return y6;
                                                                            _ -> mzero};
                                                                     x8 <- squareIO x2;
                                                                     (x7,
                                                                      x9) <- addMultiplySquareAddAddAddOIOI x8 x6 gen_addOIO_x2;
                                                                     let {x11 = x7};
                                                                     let {x0 = S x11};
                                                                     x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                     return (x0, x1)}]
addMultiplySquareAddAddAddOIOI x1 x3 gen_addOIO_x2 = msum [do {let {x0 = O};
                                                               let {x21 = O};
                                                               x2 <- squareAddAddIIOI x3 x1 x21;
                                                               return (x0, x2)},
                                                           do {x4 <- case x3 of
                                                                     {S y4 -> return y4;
                                                                      _ -> mzero};
                                                               (x5,
                                                                x2,
                                                                x22) <- squareAddAddOIOO x1 gen_addOIO_x2;
                                                               x6 <- case x22 of
                                                                     {S y6 -> return y6;
                                                                      _ -> mzero};
                                                               let {x23 = x6};
                                                               let {x0 = S x23};
                                                               let {x25 = x6};
                                                               let {x24 = S x25};
                                                               let {x26 = x6};
                                                               x7 <- _addMultiplyIOII x6 x24 x26;
                                                               let {x28 = S x7};
                                                               let {x27 = S x28};
                                                               addAddIIII x5 x6 x27 x4;
                                                               return (x0, x2)}]
squareAddAddOIOO x1 gen_addOIO_x2 = msum [do {let {x3 = O};
                                              (x29, x0) <- addOIO x1 gen_addOIO_x2;
                                              x2 <- case x29 of
                                                    {S y2 -> return y2; _ -> mzero};
                                              return (x0, x2, x3)},
                                          do {(x0,
                                               x2,
                                               x4) <- addMultiplyAddAddOIOO x1 gen_addOIO_x2;
                                              let {x3 = S x4};
                                              return (x0, x2, x3)}]
addMultiplyAddAddOIOO x1 gen_addOIO_x2 = msum [do {let {x3 = O};
                                                   (x30, x0) <- addOIO x1 gen_addOIO_x2;
                                                   x31 <- case x30 of
                                                          {S y31 -> return y31; _ -> mzero};
                                                   x32 <- case x31 of
                                                          {S y32 -> return y32; _ -> mzero};
                                                   x33 <- case x32 of
                                                          {S y33 -> return y33; _ -> mzero};
                                                   x2 <- case x33 of
                                                         {S y2 -> return y2; _ -> mzero};
                                                   return (x0, x2, x3)},
                                               do {(x0,
                                                    x2,
                                                    x4) <- addAddAddMultiplyAddAddOIOO x1 gen_addOIO_x2;
                                                   let {x3 = S x4};
                                                   return (x0, x2, x3)}]
addAddAddMultiplyAddAddOIOO x1 gen_addOIO_x2 = msum [do {let {x3 = O};
                                                         (x34, x0) <- addOIO x1 gen_addOIO_x2;
                                                         x35 <- case x34 of
                                                                {S y35 -> return y35; _ -> mzero};
                                                         x36 <- case x35 of
                                                                {S y36 -> return y36; _ -> mzero};
                                                         x37 <- case x36 of
                                                                {S y37 -> return y37; _ -> mzero};
                                                         x38 <- case x37 of
                                                                {S y38 -> return y38; _ -> mzero};
                                                         x39 <- case x38 of
                                                                {S y39 -> return y39; _ -> mzero};
                                                         x40 <- case x39 of
                                                                {S y40 -> return y40; _ -> mzero};
                                                         x41 <- case x40 of
                                                                {S y41 -> return y41; _ -> mzero};
                                                         x42 <- case x41 of
                                                                {S y42 -> return y42; _ -> mzero};
                                                         x2 <- case x42 of
                                                               {S y2 -> return y2; _ -> mzero};
                                                         return (x0, x2, x3)},
                                                     do {(x60, x0) <- addOIO x1 gen_addOIO_x2;
                                                         x61 <- case x60 of
                                                                {S y61 -> return y61; _ -> mzero};
                                                         x62 <- case x61 of
                                                                {S y62 -> return y62; _ -> mzero};
                                                         x63 <- case x62 of
                                                                {S y63 -> return y63; _ -> mzero};
                                                         x7 <- case x63 of
                                                               {S y7 -> return y7; _ -> mzero};
                                                         (x2, x4, x48) <- addAddOOOI x7;
                                                         let {x3 = S x4};
                                                         x49 <- case x48 of
                                                                {S y49 -> return y49; _ -> mzero};
                                                         x50 <- case x49 of
                                                                {S y50 -> return y50; _ -> mzero};
                                                         x51 <- case x50 of
                                                                {S y51 -> return y51; _ -> mzero};
                                                         x6 <- case x51 of
                                                               {S y6 -> return y6; _ -> mzero};
                                                         let {x46 = x4};
                                                         let {x45 = S x46};
                                                         let {x44 = S x45};
                                                         let {x43 = S x44};
                                                         let {x47 = x4};
                                                         x5 <- _addMultiplyIOII x4 x43 x47;
                                                         let {x59 = S x5};
                                                         let {x58 = S x59};
                                                         let {x57 = S x58};
                                                         let {x56 = S x57};
                                                         x52 <- addOII x4 x6;
                                                         x53 <- case x52 of
                                                                {S y53 -> return y53; _ -> mzero};
                                                         x54 <- case x53 of
                                                                {S y54 -> return y54; _ -> mzero};
                                                         x55 <- case x54 of
                                                                {S y55 -> return y55; _ -> mzero};
                                                         x8 <- case x55 of
                                                               {S y8 -> return y8; _ -> mzero};
                                                         addIII x56 x4 x8;
                                                         return (x0, x2, x3)}]
addAddOOOI x3 = msum [do {let {x1 = O};
                          (x0, x2) <- addOOI x3;
                          return (x0, x1, x2)},
                      do {x4 <- case x3 of
                                {S y4 -> return y4; _ -> mzero};
                          (x0, x5, x2) <- addAddOOOI x4;
                          let {x1 = S x5};
                          return (x0, x1, x2)}]
sumtrsquaretrOOIO x2 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 = msum [do {let {x0 = O};
                                                                                                               let {x10 = O};
                                                                                                               (x4,
                                                                                                                x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                               x3 <- squareAddIOI x2 x4;
                                                                                                               x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                               return (x0,
                                                                                                                       x1,
                                                                                                                       x3)},
                                                                                                           do {x8 <- squareIO x2;
                                                                                                               (x7,
                                                                                                                x9,
                                                                                                                x6) <- addMultiplySquareAddAddAddOIOO x8 gen_addOIO_x2;
                                                                                                               let {x3 = S x6};
                                                                                                               let {x11 = x7};
                                                                                                               let {x0 = S x11};
                                                                                                               x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                               return (x0,
                                                                                                                       x1,
                                                                                                                       x3)}]
addMultiplySquareAddAddAddOIOO x1 gen_addOIO_x2 = msum [do {let {x0 = O};
                                                            let {x21 = O};
                                                            (x3,
                                                             x2) <- squareAddAddOIOI x1 x21 gen_addOIO_x2;
                                                            return (x0, x2, x3)},
                                                        do {(x5,
                                                             x2,
                                                             x22) <- squareAddAddOIOO x1 gen_addOIO_x2;
                                                            x6 <- case x22 of
                                                                  {S y6 -> return y6; _ -> mzero};
                                                            let {x23 = x6};
                                                            let {x0 = S x23};
                                                            let {x25 = x6};
                                                            let {x24 = S x25};
                                                            let {x26 = x6};
                                                            x7 <- _addMultiplyIOII x6 x24 x26;
                                                            let {x28 = S x7};
                                                            let {x27 = S x28};
                                                            x4 <- addAddIIIO x5 x6 x27;
                                                            let {x3 = S x4};
                                                            return (x0, x2, x3)}]
sumtrsquaretrOOOI x3 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 = msum [do {let {x0 = O};
                                                                                                               let {x10 = O};
                                                                                                               (x4,
                                                                                                                x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                               x2 <- squareAddOII x3 x4;
                                                                                                               x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                               return (x0,
                                                                                                                       x1,
                                                                                                                       x2)},
                                                                                                           do {x6 <- case x3 of
                                                                                                                     {S y6 -> return y6;
                                                                                                                      _ -> mzero};
                                                                                                               (x7,
                                                                                                                x8,
                                                                                                                x9) <- addMultiplySquareAddAddAddOOOI x6 gen_addOIO_x2;
                                                                                                               let {x11 = x7};
                                                                                                               let {x0 = S x11};
                                                                                                               x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                                                                               x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                               return (x0,
                                                                                                                       x1,
                                                                                                                       x2)}]
addMultiplySquareAddAddAddOOOI x3 gen_addOIO_x2 = msum [do {let {x0 = O};
                                                            let {x21 = O};
                                                            (x1,
                                                             x2) <- squareAddAddIOOI x3 x21 gen_addOIO_x2;
                                                            return (x0, x1, x2)},
                                                        do {x4 <- case x3 of
                                                                  {S y4 -> return y4; _ -> mzero};
                                                            (x5, x6, x27) <- addAddOOOI x4;
                                                            let {x22 = S x6};
                                                            x28 <- case x27 of
                                                                   {S y28 -> return y28;
                                                                    _ -> mzero};
                                                            x7 <- case x28 of
                                                                  {S y7 -> return y7; _ -> mzero};
                                                            let {x23 = x6};
                                                            let {x0 = S x23};
                                                            let {x25 = x6};
                                                            let {x24 = S x25};
                                                            let {x26 = x6};
                                                            _addMultiplyIIII x6 x7 x24 x26;
                                                            (x1,
                                                             x2) <- squareAddAddIOOI x5 x22 gen_addOIO_x2;
                                                            return (x0, x1, x2)}]
sumtrsquaretrOOOO gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_addOOO_x2 gen_multiplyOOI_x0 gen_squareAddIOO_x2 gen_sumtrsquaretrOOOO_x7 = msum [do {let {x0 = O};
                                                                                                                                                   let {x10 = O};
                                                                                                                                                   (x4,
                                                                                                                                                    x5) <- squareAddIOO x10 gen_addMultiplyAddOIO_x2 gen_addOIO_x2 gen_squareAddIOO_x2;
                                                                                                                                                   x1 <- squareOI x5 gen_multiplyOOI_x0;
                                                                                                                                                   (x2,
                                                                                                                                                    x3) <- squareAddOOI x4;
                                                                                                                                                   return (x0,
                                                                                                                                                           x1,
                                                                                                                                                           x2,
                                                                                                                                                           x3)},
                                                                                                                                               do {(x11,
                                                                                                                                                    x7) <- do {x7 <- gen_sumtrsquaretrOOOO_x7;
                                                                                                                                                               return (x7,
                                                                                                                                                                       x7)};
                                                                                                                                                   let {x0 = S x11};
                                                                                                                                                   (x8,
                                                                                                                                                    x9,
                                                                                                                                                    x6) <- addMultiplySquareAddAddAddIOOO x7 gen_addOIO_x2 gen_addOOO_x2;
                                                                                                                                                   let {x3 = S x6};
                                                                                                                                                   x2 <- squareOI x8 gen_multiplyOOI_x0;
                                                                                                                                                   x1 <- squareOI x9 gen_multiplyOOI_x0;
                                                                                                                                                   return (x0,
                                                                                                                                                           x1,
                                                                                                                                                           x2,
                                                                                                                                                           x3)}]