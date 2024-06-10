module Multiply_clean where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
multiplyII x0 x1 = msum [do {let {x4 = O};
                             x2 <- case x1 of
                                   {S y2 -> return y2; _ -> mzero};
                             multiplyII x4 x2;
                             guard (x0 == O);
                             return ()},
                         do {x2 <- case x1 of
                                   {S y2 -> return y2; _ -> mzero};
                             x3 <- case x0 of
                                   {S y3 -> return y3; _ -> mzero};
                             addMultiplyII x2 x3;
                             return ()}]
addMultiplyII x0 x1 = msum [do {guard (x1 == O);
                                x19 <- case x0 of
                                       {S y19 -> return y19; _ -> mzero};
                                let {x5 = x19};
                                x6 <- case x5 of
                                      {S y6 -> return y6; _ -> mzero};
                                x7 <- case x6 of
                                      {S y7 -> return y7; _ -> mzero};
                                x8 <- case x7 of
                                      {S y8 -> return y8; _ -> mzero};
                                x9 <- case x8 of
                                      {S y9 -> return y9; _ -> mzero};
                                x10 <- case x9 of
                                       {S y10 -> return y10; _ -> mzero};
                                x11 <- case x10 of
                                       {S y11 -> return y11; _ -> mzero};
                                x12 <- case x11 of
                                       {S y12 -> return y12; _ -> mzero};
                                x13 <- case x12 of
                                       {S y13 -> return y13; _ -> mzero};
                                x14 <- case x13 of
                                       {S y14 -> return y14; _ -> mzero};
                                x15 <- case x14 of
                                       {S y15 -> return y15; _ -> mzero};
                                x16 <- case x15 of
                                       {S y16 -> return y16; _ -> mzero};
                                x17 <- case x16 of
                                       {S y17 -> return y17; _ -> mzero};
                                x18 <- case x17 of
                                       {S y18 -> return y18; _ -> mzero};
                                x2 <- case x18 of
                                      {S y2 -> return y2; _ -> mzero};
                                _multiplyI x2;
                                return ()},
                            do {x3 <- case x1 of
                                      {S y3 -> return y3; _ -> mzero};
                                _addMultiplyII x0 x3;
                                return ()}]
_addMultiplyII x0 x1 = msum [do {guard (x1 == O);
                                 x26 <- case x0 of
                                        {S y26 -> return y26; _ -> mzero};
                                 let {x20 = x26};
                                 x21 <- case x20 of
                                        {S y21 -> return y21; _ -> mzero};
                                 x22 <- case x21 of
                                        {S y22 -> return y22; _ -> mzero};
                                 x23 <- case x22 of
                                        {S y23 -> return y23; _ -> mzero};
                                 x24 <- case x23 of
                                        {S y24 -> return y24; _ -> mzero};
                                 x25 <- case x24 of
                                        {S y25 -> return y25; _ -> mzero};
                                 x2 <- case x25 of
                                       {S y2 -> return y2; _ -> mzero};
                                 __multiplyI x2;
                                 return ()},
                             do {x3 <- case x1 of
                                       {S y3 -> return y3; _ -> mzero};
                                 __addMultiplyII x0 x3;
                                 return ()}]
__addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                        {S y2 -> return y2; _ -> mzero};
                                  ___addMultiplyII x0 x2;
                                  return ()}]
___addMultiplyII x0 x1 = msum [do {guard (x1 == O);
                                   x29 <- case x0 of
                                          {S y29 -> return y29; _ -> mzero};
                                   let {x27 = x29};
                                   x28 <- case x27 of
                                          {S y28 -> return y28; _ -> mzero};
                                   x2 <- case x28 of
                                         {S y2 -> return y2; _ -> mzero};
                                   ___multiplyI x2;
                                   return ()},
                               do {x3 <- case x1 of
                                         {S y3 -> return y3; _ -> mzero};
                                   ____addMultiplyII x0 x3;
                                   return ()}]
____addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                          {S y2 -> return y2; _ -> mzero};
                                    _____addMultiplyII x0 x2;
                                    return ()}]
_____addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                           {S y2 -> return y2; _ -> mzero};
                                     ______addMultiplyII x0 x2;
                                     return ()}]
______addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                            {S y2 -> return y2; _ -> mzero};
                                      _______addMultiplyII x0 x2;
                                      return ()}]
_______addMultiplyII x0 x1 = msum [do {guard (x1 == O);
                                       x2 <- case x0 of
                                             {S y2 -> return y2; _ -> mzero};
                                       ____multiplyI x2;
                                       return ()},
                                   do {x3 <- case x1 of
                                             {S y3 -> return y3; _ -> mzero};
                                       ________addMultiplyII x0 x3;
                                       return ()}]
________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                              {S y2 -> return y2; _ -> mzero};
                                        _________addMultiplyII x0 x2;
                                        return ()}]
_________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                               {S y2 -> return y2; _ -> mzero};
                                         __________addMultiplyII x0 x2;
                                         return ()}]
__________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                                {S y2 -> return y2; _ -> mzero};
                                          ___________addMultiplyII x0 x2;
                                          return ()}]
___________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                                 {S y2 -> return y2; _ -> mzero};
                                           ____________addMultiplyII x0 x2;
                                           return ()}]
____________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                                  {S y2 -> return y2; _ -> mzero};
                                            _____________addMultiplyII x0 x2;
                                            return ()}]
_____________addMultiplyII x0 x1 = msum [do {x2 <- case x1 of
                                                   {S y2 -> return y2; _ -> mzero};
                                             ______________addMultiplyII x0 x2;
                                             return ()}]
______________addMultiplyII x0 x1 = msum [do {_____multiplyI x0;
                                              let {x30 = O};
                                              x31 <- case x1 of
                                                     {S y31 -> return y31; _ -> mzero};
                                              guard (x31 == x30);
                                              return ()}]
_____multiplyI x0 = msum [do {guard (x0 == O); return ()}]
____multiplyI x0 = msum [do {guard (x0 == O); return ()}]
___multiplyI x0 = msum [do {guard (x0 == O); return ()}]
__multiplyI x0 = msum [do {guard (x0 == O); return ()}]
_multiplyI x0 = msum [do {guard (x0 == O); return ()}]
multiplyIO x0 = msum [do {let {x4 = O};
                          guard (x0 == O);
                          x2 <- multiplyIO x4;
                          let {x1 = S x2};
                          return x1},
                      do {x3 <- case x0 of
                                {S y3 -> return y3; _ -> mzero};
                          x2 <- addMultiplyOI x3;
                          let {x1 = S x2};
                          return x1}]
addMultiplyOI x1 = msum [do {guard (x1 == O);
                             x2 <- _multiplyO;
                             let {x18 = S x2};
                             let {x17 = S x18};
                             let {x16 = S x17};
                             let {x15 = S x16};
                             let {x14 = S x15};
                             let {x13 = S x14};
                             let {x12 = S x13};
                             let {x11 = S x12};
                             let {x10 = S x11};
                             let {x9 = S x10};
                             let {x8 = S x9};
                             let {x7 = S x8};
                             let {x6 = S x7};
                             let {x5 = S x6};
                             let {x19 = x5};
                             let {x0 = S x19};
                             return x0},
                         do {x3 <- case x1 of
                                   {S y3 -> return y3; _ -> mzero};
                             x0 <- _addMultiplyOI x3;
                             return x0}]
_addMultiplyOI x1 = msum [do {guard (x1 == O);
                              x2 <- __multiplyO;
                              let {x25 = S x2};
                              let {x24 = S x25};
                              let {x23 = S x24};
                              let {x22 = S x23};
                              let {x21 = S x22};
                              let {x20 = S x21};
                              let {x26 = x20};
                              let {x0 = S x26};
                              return x0},
                          do {x3 <- case x1 of
                                    {S y3 -> return y3; _ -> mzero};
                              x0 <- __addMultiplyOI x3;
                              return x0}]
__addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                     {S y2 -> return y2; _ -> mzero};
                               x0 <- ___addMultiplyOI x2;
                               return x0}]
___addMultiplyOI x1 = msum [do {guard (x1 == O);
                                x2 <- ___multiplyO;
                                let {x28 = S x2};
                                let {x27 = S x28};
                                let {x29 = x27};
                                let {x0 = S x29};
                                return x0},
                            do {x3 <- case x1 of
                                      {S y3 -> return y3; _ -> mzero};
                                x0 <- ____addMultiplyOI x3;
                                return x0}]
____addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                       {S y2 -> return y2; _ -> mzero};
                                 x0 <- _____addMultiplyOI x2;
                                 return x0}]
_____addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                        {S y2 -> return y2; _ -> mzero};
                                  x0 <- ______addMultiplyOI x2;
                                  return x0}]
______addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                         {S y2 -> return y2; _ -> mzero};
                                   x0 <- _______addMultiplyOI x2;
                                   return x0}]
_______addMultiplyOI x1 = msum [do {guard (x1 == O);
                                    x2 <- ____multiplyO;
                                    let {x0 = S x2};
                                    return x0},
                                do {x3 <- case x1 of
                                          {S y3 -> return y3; _ -> mzero};
                                    x0 <- ________addMultiplyOI x3;
                                    return x0}]
________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                           {S y2 -> return y2; _ -> mzero};
                                     x0 <- _________addMultiplyOI x2;
                                     return x0}]
_________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                            {S y2 -> return y2; _ -> mzero};
                                      x0 <- __________addMultiplyOI x2;
                                      return x0}]
__________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                             {S y2 -> return y2; _ -> mzero};
                                       x0 <- ___________addMultiplyOI x2;
                                       return x0}]
___________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                              {S y2 -> return y2; _ -> mzero};
                                        x0 <- ____________addMultiplyOI x2;
                                        return x0}]
____________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                               {S y2 -> return y2; _ -> mzero};
                                         x0 <- _____________addMultiplyOI x2;
                                         return x0}]
_____________addMultiplyOI x1 = msum [do {x2 <- case x1 of
                                                {S y2 -> return y2; _ -> mzero};
                                          x0 <- ______________addMultiplyOI x2;
                                          return x0}]
______________addMultiplyOI x1 = msum [do {let {x30 = O};
                                           x31 <- case x1 of
                                                  {S y31 -> return y31; _ -> mzero};
                                           guard (x31 == x30);
                                           x0 <- _____multiplyO;
                                           return x0}]
_____multiplyO = msum [do {let {x0 = O}; return x0}]
____multiplyO = msum [do {let {x0 = O}; return x0}]
___multiplyO = msum [do {let {x0 = O}; return x0}]
__multiplyO = msum [do {let {x0 = O}; return x0}]
_multiplyO = msum [do {let {x0 = O}; return x0}]
multiplyOI x1 = msum [do {let {x0 = O};
                          let {x4 = O};
                          x2 <- case x1 of
                                {S y2 -> return y2; _ -> mzero};
                          multiplyII x4 x2;
                          return x0},
                      do {x2 <- case x1 of
                                {S y2 -> return y2; _ -> mzero};
                          x3 <- addMultiplyIO x2;
                          let {x0 = S x3};
                          return x0}]
addMultiplyIO x0 = msum [do {let {x1 = O};
                             x19 <- case x0 of
                                    {S y19 -> return y19; _ -> mzero};
                             let {x5 = x19};
                             x6 <- case x5 of
                                   {S y6 -> return y6; _ -> mzero};
                             x7 <- case x6 of
                                   {S y7 -> return y7; _ -> mzero};
                             x8 <- case x7 of
                                   {S y8 -> return y8; _ -> mzero};
                             x9 <- case x8 of
                                   {S y9 -> return y9; _ -> mzero};
                             x10 <- case x9 of
                                    {S y10 -> return y10; _ -> mzero};
                             x11 <- case x10 of
                                    {S y11 -> return y11; _ -> mzero};
                             x12 <- case x11 of
                                    {S y12 -> return y12; _ -> mzero};
                             x13 <- case x12 of
                                    {S y13 -> return y13; _ -> mzero};
                             x14 <- case x13 of
                                    {S y14 -> return y14; _ -> mzero};
                             x15 <- case x14 of
                                    {S y15 -> return y15; _ -> mzero};
                             x16 <- case x15 of
                                    {S y16 -> return y16; _ -> mzero};
                             x17 <- case x16 of
                                    {S y17 -> return y17; _ -> mzero};
                             x18 <- case x17 of
                                    {S y18 -> return y18; _ -> mzero};
                             x2 <- case x18 of
                                   {S y2 -> return y2; _ -> mzero};
                             _multiplyI x2;
                             return x1},
                         do {x3 <- _addMultiplyIO x0; let {x1 = S x3}; return x1}]
_addMultiplyIO x0 = msum [do {let {x1 = O};
                              x26 <- case x0 of
                                     {S y26 -> return y26; _ -> mzero};
                              let {x20 = x26};
                              x21 <- case x20 of
                                     {S y21 -> return y21; _ -> mzero};
                              x22 <- case x21 of
                                     {S y22 -> return y22; _ -> mzero};
                              x23 <- case x22 of
                                     {S y23 -> return y23; _ -> mzero};
                              x24 <- case x23 of
                                     {S y24 -> return y24; _ -> mzero};
                              x25 <- case x24 of
                                     {S y25 -> return y25; _ -> mzero};
                              x2 <- case x25 of
                                    {S y2 -> return y2; _ -> mzero};
                              __multiplyI x2;
                              return x1},
                          do {x3 <- __addMultiplyIO x0; let {x1 = S x3}; return x1}]
__addMultiplyIO x0 = msum [do {x2 <- ___addMultiplyIO x0;
                               let {x1 = S x2};
                               return x1}]
___addMultiplyIO x0 = msum [do {let {x1 = O};
                                x29 <- case x0 of
                                       {S y29 -> return y29; _ -> mzero};
                                let {x27 = x29};
                                x28 <- case x27 of
                                       {S y28 -> return y28; _ -> mzero};
                                x2 <- case x28 of
                                      {S y2 -> return y2; _ -> mzero};
                                ___multiplyI x2;
                                return x1},
                            do {x3 <- ____addMultiplyIO x0; let {x1 = S x3}; return x1}]
____addMultiplyIO x0 = msum [do {x2 <- _____addMultiplyIO x0;
                                 let {x1 = S x2};
                                 return x1}]
_____addMultiplyIO x0 = msum [do {x2 <- ______addMultiplyIO x0;
                                  let {x1 = S x2};
                                  return x1}]
______addMultiplyIO x0 = msum [do {x2 <- _______addMultiplyIO x0;
                                   let {x1 = S x2};
                                   return x1}]
_______addMultiplyIO x0 = msum [do {let {x1 = O};
                                    x2 <- case x0 of
                                          {S y2 -> return y2; _ -> mzero};
                                    ____multiplyI x2;
                                    return x1},
                                do {x3 <- ________addMultiplyIO x0; let {x1 = S x3}; return x1}]
________addMultiplyIO x0 = msum [do {x2 <- _________addMultiplyIO x0;
                                     let {x1 = S x2};
                                     return x1}]
_________addMultiplyIO x0 = msum [do {x2 <- __________addMultiplyIO x0;
                                      let {x1 = S x2};
                                      return x1}]
__________addMultiplyIO x0 = msum [do {x2 <- ___________addMultiplyIO x0;
                                       let {x1 = S x2};
                                       return x1}]
___________addMultiplyIO x0 = msum [do {x2 <- ____________addMultiplyIO x0;
                                        let {x1 = S x2};
                                        return x1}]
____________addMultiplyIO x0 = msum [do {x2 <- _____________addMultiplyIO x0;
                                         let {x1 = S x2};
                                         return x1}]
_____________addMultiplyIO x0 = msum [do {x2 <- ______________addMultiplyIO x0;
                                          let {x1 = S x2};
                                          return x1}]
______________addMultiplyIO x0 = msum [do {_____multiplyI x0;
                                           let {x30 = O};
                                           let {x31 = x30};
                                           let {x1 = S x31};
                                           return x1}]
multiplyOO = msum [do {let {x0 = O};
                       let {x4 = O};
                       x2 <- multiplyIO x4;
                       let {x1 = S x2};
                       return (x0, x1)},
                   do {(x2, x3) <- addMultiplyOO;
                       let {x1 = S x2};
                       let {x0 = S x3};
                       return (x0, x1)}]
addMultiplyOO = msum [do {let {x1 = O};
                          x2 <- _multiplyO;
                          let {x18 = S x2};
                          let {x17 = S x18};
                          let {x16 = S x17};
                          let {x15 = S x16};
                          let {x14 = S x15};
                          let {x13 = S x14};
                          let {x12 = S x13};
                          let {x11 = S x12};
                          let {x10 = S x11};
                          let {x9 = S x10};
                          let {x8 = S x9};
                          let {x7 = S x8};
                          let {x6 = S x7};
                          let {x5 = S x6};
                          let {x19 = x5};
                          let {x0 = S x19};
                          return (x0, x1)},
                      do {(x0, x3) <- _addMultiplyOO; let {x1 = S x3}; return (x0, x1)}]
_addMultiplyOO = msum [do {let {x1 = O};
                           x2 <- __multiplyO;
                           let {x25 = S x2};
                           let {x24 = S x25};
                           let {x23 = S x24};
                           let {x22 = S x23};
                           let {x21 = S x22};
                           let {x20 = S x21};
                           let {x26 = x20};
                           let {x0 = S x26};
                           return (x0, x1)},
                       do {(x0, x3) <- __addMultiplyOO; let {x1 = S x3}; return (x0, x1)}]
__addMultiplyOO = msum [do {(x0, x2) <- ___addMultiplyOO;
                            let {x1 = S x2};
                            return (x0, x1)}]
___addMultiplyOO = msum [do {let {x1 = O};
                             x2 <- ___multiplyO;
                             let {x28 = S x2};
                             let {x27 = S x28};
                             let {x29 = x27};
                             let {x0 = S x29};
                             return (x0, x1)},
                         do {(x0, x3) <- ____addMultiplyOO;
                             let {x1 = S x3};
                             return (x0, x1)}]
____addMultiplyOO = msum [do {(x0, x2) <- _____addMultiplyOO;
                              let {x1 = S x2};
                              return (x0, x1)}]
_____addMultiplyOO = msum [do {(x0, x2) <- ______addMultiplyOO;
                               let {x1 = S x2};
                               return (x0, x1)}]
______addMultiplyOO = msum [do {(x0, x2) <- _______addMultiplyOO;
                                let {x1 = S x2};
                                return (x0, x1)}]
_______addMultiplyOO = msum [do {let {x1 = O};
                                 x2 <- ____multiplyO;
                                 let {x0 = S x2};
                                 return (x0, x1)},
                             do {(x0, x3) <- ________addMultiplyOO;
                                 let {x1 = S x3};
                                 return (x0, x1)}]
________addMultiplyOO = msum [do {(x0,
                                   x2) <- _________addMultiplyOO;
                                  let {x1 = S x2};
                                  return (x0, x1)}]
_________addMultiplyOO = msum [do {(x0,
                                    x2) <- __________addMultiplyOO;
                                   let {x1 = S x2};
                                   return (x0, x1)}]
__________addMultiplyOO = msum [do {(x0,
                                     x2) <- ___________addMultiplyOO;
                                    let {x1 = S x2};
                                    return (x0, x1)}]
___________addMultiplyOO = msum [do {(x0,
                                      x2) <- ____________addMultiplyOO;
                                     let {x1 = S x2};
                                     return (x0, x1)}]
____________addMultiplyOO = msum [do {(x0,
                                       x2) <- _____________addMultiplyOO;
                                      let {x1 = S x2};
                                      return (x0, x1)}]
_____________addMultiplyOO = msum [do {(x0,
                                        x2) <- ______________addMultiplyOO;
                                       let {x1 = S x2};
                                       return (x0, x1)}]
______________addMultiplyOO = msum [do {let {x30 = O};
                                        let {x31 = x30};
                                        let {x1 = S x31};
                                        x0 <- _____multiplyO;
                                        return (x0, x1)}]