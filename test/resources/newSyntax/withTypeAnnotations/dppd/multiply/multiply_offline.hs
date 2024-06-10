module Multiply_clean where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
multiplyddsII x0 x1 = msum [do {x8 <- case x1 of
                                      {S y8 -> return y8; _ -> mzero};
                                guard (x0 == O);
                                let {x7 = x8};
                                x2 <- case x7 of
                                      {S y2 -> return y2; _ -> mzero};
                                multiplysdsI x2;
                                return ()},
                            do {let {x25 = O};
                                x24 <- case x1 of
                                       {S y24 -> return y24; _ -> mzero};
                                x26 <- case x0 of
                                       {S y26 -> return y26; _ -> mzero};
                                guard (x26 == x25);
                                let {x9 = x24};
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
                                x19 <- case x18 of
                                       {S y19 -> return y19; _ -> mzero};
                                x20 <- case x19 of
                                       {S y20 -> return y20; _ -> mzero};
                                x21 <- case x20 of
                                       {S y21 -> return y21; _ -> mzero};
                                x22 <- case x21 of
                                       {S y22 -> return y22; _ -> mzero};
                                x23 <- case x22 of
                                       {S y23 -> return y23; _ -> mzero};
                                x3 <- case x23 of
                                      {S y3 -> return y3; _ -> mzero};
                                _multiplysdsI x3;
                                return ()},
                            do {let {x36 = O};
                                let {x35 = S x36};
                                x34 <- case x1 of
                                       {S y34 -> return y34; _ -> mzero};
                                x37 <- case x0 of
                                       {S y37 -> return y37; _ -> mzero};
                                guard (x37 == x35);
                                let {x27 = x34};
                                x28 <- case x27 of
                                       {S y28 -> return y28; _ -> mzero};
                                x29 <- case x28 of
                                       {S y29 -> return y29; _ -> mzero};
                                x30 <- case x29 of
                                       {S y30 -> return y30; _ -> mzero};
                                x31 <- case x30 of
                                       {S y31 -> return y31; _ -> mzero};
                                x32 <- case x31 of
                                       {S y32 -> return y32; _ -> mzero};
                                x33 <- case x32 of
                                       {S y33 -> return y33; _ -> mzero};
                                x4 <- case x33 of
                                      {S y4 -> return y4; _ -> mzero};
                                __multiplysdsI x4;
                                return ()},
                            do {let {x45 = O};
                                let {x44 = S x45};
                                let {x43 = S x44};
                                let {x42 = S x43};
                                x41 <- case x1 of
                                       {S y41 -> return y41; _ -> mzero};
                                x46 <- case x0 of
                                       {S y46 -> return y46; _ -> mzero};
                                guard (x46 == x42);
                                let {x38 = x41};
                                x39 <- case x38 of
                                       {S y39 -> return y39; _ -> mzero};
                                x40 <- case x39 of
                                       {S y40 -> return y40; _ -> mzero};
                                x5 <- case x40 of
                                      {S y5 -> return y5; _ -> mzero};
                                ___multiplysdsI x5;
                                return ()},
                            do {let {x56 = O};
                                let {x55 = S x56};
                                let {x54 = S x55};
                                let {x53 = S x54};
                                let {x52 = S x53};
                                let {x51 = S x52};
                                let {x50 = S x51};
                                let {x49 = S x50};
                                x48 <- case x1 of
                                       {S y48 -> return y48; _ -> mzero};
                                x57 <- case x0 of
                                       {S y57 -> return y57; _ -> mzero};
                                guard (x57 == x49);
                                let {x47 = x48};
                                x6 <- case x47 of
                                      {S y6 -> return y6; _ -> mzero};
                                ____multiplysdsI x6;
                                return ()},
                            do {let {x58 = O};
                                let {x75 = O};
                                let {x74 = S x75};
                                let {x73 = S x74};
                                let {x72 = S x73};
                                let {x71 = S x72};
                                let {x70 = S x71};
                                let {x69 = S x70};
                                let {x68 = S x69};
                                let {x67 = S x68};
                                let {x66 = S x67};
                                let {x65 = S x66};
                                let {x64 = S x65};
                                let {x63 = S x64};
                                let {x62 = S x63};
                                let {x61 = S x62};
                                let {x60 = S x61};
                                x59 <- case x1 of
                                       {S y59 -> return y59; _ -> mzero};
                                guard (x59 == x58);
                                x76 <- case x0 of
                                       {S y76 -> return y76; _ -> mzero};
                                guard (x76 == x60);
                                return ()}]
____multiplysdsI x0 = msum [do {guard (x0 == O); return ()}]
___multiplysdsI x0 = msum [do {guard (x0 == O); return ()}]
__multiplysdsI x0 = msum [do {guard (x0 == O); return ()}]
_multiplysdsI x0 = msum [do {guard (x0 == O); return ()}]
multiplyddsIO x0 = msum [do {guard (x0 == O);
                             x2 <- multiplysdsO;
                             let {x7 = S x2};
                             let {x8 = x7};
                             let {x1 = S x8};
                             return x1},
                         do {let {x25 = O};
                             x26 <- case x0 of
                                    {S y26 -> return y26; _ -> mzero};
                             guard (x26 == x25);
                             x3 <- _multiplysdsO;
                             let {x23 = S x3};
                             let {x22 = S x23};
                             let {x21 = S x22};
                             let {x20 = S x21};
                             let {x19 = S x20};
                             let {x18 = S x19};
                             let {x17 = S x18};
                             let {x16 = S x17};
                             let {x15 = S x16};
                             let {x14 = S x15};
                             let {x13 = S x14};
                             let {x12 = S x13};
                             let {x11 = S x12};
                             let {x10 = S x11};
                             let {x9 = S x10};
                             let {x24 = x9};
                             let {x1 = S x24};
                             return x1},
                         do {let {x36 = O};
                             let {x35 = S x36};
                             x37 <- case x0 of
                                    {S y37 -> return y37; _ -> mzero};
                             guard (x37 == x35);
                             x4 <- __multiplysdsO;
                             let {x33 = S x4};
                             let {x32 = S x33};
                             let {x31 = S x32};
                             let {x30 = S x31};
                             let {x29 = S x30};
                             let {x28 = S x29};
                             let {x27 = S x28};
                             let {x34 = x27};
                             let {x1 = S x34};
                             return x1},
                         do {let {x45 = O};
                             let {x44 = S x45};
                             let {x43 = S x44};
                             let {x42 = S x43};
                             x46 <- case x0 of
                                    {S y46 -> return y46; _ -> mzero};
                             guard (x46 == x42);
                             x5 <- ___multiplysdsO;
                             let {x40 = S x5};
                             let {x39 = S x40};
                             let {x38 = S x39};
                             let {x41 = x38};
                             let {x1 = S x41};
                             return x1},
                         do {let {x56 = O};
                             let {x55 = S x56};
                             let {x54 = S x55};
                             let {x53 = S x54};
                             let {x52 = S x53};
                             let {x51 = S x52};
                             let {x50 = S x51};
                             let {x49 = S x50};
                             x57 <- case x0 of
                                    {S y57 -> return y57; _ -> mzero};
                             guard (x57 == x49);
                             x6 <- ____multiplysdsO;
                             let {x47 = S x6};
                             let {x48 = x47};
                             let {x1 = S x48};
                             return x1},
                         do {let {x58 = O};
                             let {x75 = O};
                             let {x74 = S x75};
                             let {x73 = S x74};
                             let {x72 = S x73};
                             let {x71 = S x72};
                             let {x70 = S x71};
                             let {x69 = S x70};
                             let {x68 = S x69};
                             let {x67 = S x68};
                             let {x66 = S x67};
                             let {x65 = S x66};
                             let {x64 = S x65};
                             let {x63 = S x64};
                             let {x62 = S x63};
                             let {x61 = S x62};
                             let {x60 = S x61};
                             x76 <- case x0 of
                                    {S y76 -> return y76; _ -> mzero};
                             guard (x76 == x60);
                             let {x59 = x58};
                             let {x1 = S x59};
                             return x1}]
____multiplysdsO = msum [do {let {x0 = O}; return x0}]
___multiplysdsO = msum [do {let {x0 = O}; return x0}]
__multiplysdsO = msum [do {let {x0 = O}; return x0}]
_multiplysdsO = msum [do {let {x0 = O}; return x0}]
multiplyddsOI x1 = msum [do {let {x0 = O};
                             x8 <- case x1 of
                                   {S y8 -> return y8; _ -> mzero};
                             let {x7 = x8};
                             x2 <- case x7 of
                                   {S y2 -> return y2; _ -> mzero};
                             multiplysdsI x2;
                             return x0},
                         do {let {x25 = O};
                             x24 <- case x1 of
                                    {S y24 -> return y24; _ -> mzero};
                             let {x9 = x24};
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
                             x19 <- case x18 of
                                    {S y19 -> return y19; _ -> mzero};
                             x20 <- case x19 of
                                    {S y20 -> return y20; _ -> mzero};
                             x21 <- case x20 of
                                    {S y21 -> return y21; _ -> mzero};
                             x22 <- case x21 of
                                    {S y22 -> return y22; _ -> mzero};
                             x23 <- case x22 of
                                    {S y23 -> return y23; _ -> mzero};
                             x3 <- case x23 of
                                   {S y3 -> return y3; _ -> mzero};
                             _multiplysdsI x3;
                             let {x26 = x25};
                             let {x0 = S x26};
                             return x0},
                         do {let {x36 = O};
                             let {x35 = S x36};
                             x34 <- case x1 of
                                    {S y34 -> return y34; _ -> mzero};
                             let {x27 = x34};
                             x28 <- case x27 of
                                    {S y28 -> return y28; _ -> mzero};
                             x29 <- case x28 of
                                    {S y29 -> return y29; _ -> mzero};
                             x30 <- case x29 of
                                    {S y30 -> return y30; _ -> mzero};
                             x31 <- case x30 of
                                    {S y31 -> return y31; _ -> mzero};
                             x32 <- case x31 of
                                    {S y32 -> return y32; _ -> mzero};
                             x33 <- case x32 of
                                    {S y33 -> return y33; _ -> mzero};
                             x4 <- case x33 of
                                   {S y4 -> return y4; _ -> mzero};
                             __multiplysdsI x4;
                             let {x37 = x35};
                             let {x0 = S x37};
                             return x0},
                         do {let {x45 = O};
                             let {x44 = S x45};
                             let {x43 = S x44};
                             let {x42 = S x43};
                             x41 <- case x1 of
                                    {S y41 -> return y41; _ -> mzero};
                             let {x38 = x41};
                             x39 <- case x38 of
                                    {S y39 -> return y39; _ -> mzero};
                             x40 <- case x39 of
                                    {S y40 -> return y40; _ -> mzero};
                             x5 <- case x40 of
                                   {S y5 -> return y5; _ -> mzero};
                             ___multiplysdsI x5;
                             let {x46 = x42};
                             let {x0 = S x46};
                             return x0},
                         do {let {x56 = O};
                             let {x55 = S x56};
                             let {x54 = S x55};
                             let {x53 = S x54};
                             let {x52 = S x53};
                             let {x51 = S x52};
                             let {x50 = S x51};
                             let {x49 = S x50};
                             x48 <- case x1 of
                                    {S y48 -> return y48; _ -> mzero};
                             let {x47 = x48};
                             x6 <- case x47 of
                                   {S y6 -> return y6; _ -> mzero};
                             ____multiplysdsI x6;
                             let {x57 = x49};
                             let {x0 = S x57};
                             return x0},
                         do {let {x58 = O};
                             let {x75 = O};
                             let {x74 = S x75};
                             let {x73 = S x74};
                             let {x72 = S x73};
                             let {x71 = S x72};
                             let {x70 = S x71};
                             let {x69 = S x70};
                             let {x68 = S x69};
                             let {x67 = S x68};
                             let {x66 = S x67};
                             let {x65 = S x66};
                             let {x64 = S x65};
                             let {x63 = S x64};
                             let {x62 = S x63};
                             let {x61 = S x62};
                             let {x60 = S x61};
                             x59 <- case x1 of
                                    {S y59 -> return y59; _ -> mzero};
                             guard (x59 == x58);
                             let {x76 = x60};
                             let {x0 = S x76};
                             return x0}]
multiplyddsOO = msum [do {let {x0 = O};
                          x2 <- multiplysdsO;
                          let {x7 = S x2};
                          let {x8 = x7};
                          let {x1 = S x8};
                          return (x0, x1)},
                      do {let {x25 = O};
                          let {x26 = x25};
                          let {x0 = S x26};
                          x3 <- _multiplysdsO;
                          let {x23 = S x3};
                          let {x22 = S x23};
                          let {x21 = S x22};
                          let {x20 = S x21};
                          let {x19 = S x20};
                          let {x18 = S x19};
                          let {x17 = S x18};
                          let {x16 = S x17};
                          let {x15 = S x16};
                          let {x14 = S x15};
                          let {x13 = S x14};
                          let {x12 = S x13};
                          let {x11 = S x12};
                          let {x10 = S x11};
                          let {x9 = S x10};
                          let {x24 = x9};
                          let {x1 = S x24};
                          return (x0, x1)},
                      do {let {x36 = O};
                          let {x35 = S x36};
                          let {x37 = x35};
                          let {x0 = S x37};
                          x4 <- __multiplysdsO;
                          let {x33 = S x4};
                          let {x32 = S x33};
                          let {x31 = S x32};
                          let {x30 = S x31};
                          let {x29 = S x30};
                          let {x28 = S x29};
                          let {x27 = S x28};
                          let {x34 = x27};
                          let {x1 = S x34};
                          return (x0, x1)},
                      do {let {x45 = O};
                          let {x44 = S x45};
                          let {x43 = S x44};
                          let {x42 = S x43};
                          let {x46 = x42};
                          let {x0 = S x46};
                          x5 <- ___multiplysdsO;
                          let {x40 = S x5};
                          let {x39 = S x40};
                          let {x38 = S x39};
                          let {x41 = x38};
                          let {x1 = S x41};
                          return (x0, x1)},
                      do {let {x56 = O};
                          let {x55 = S x56};
                          let {x54 = S x55};
                          let {x53 = S x54};
                          let {x52 = S x53};
                          let {x51 = S x52};
                          let {x50 = S x51};
                          let {x49 = S x50};
                          let {x57 = x49};
                          let {x0 = S x57};
                          x6 <- ____multiplysdsO;
                          let {x47 = S x6};
                          let {x48 = x47};
                          let {x1 = S x48};
                          return (x0, x1)},
                      do {let {x58 = O};
                          let {x75 = O};
                          let {x74 = S x75};
                          let {x73 = S x74};
                          let {x72 = S x73};
                          let {x71 = S x72};
                          let {x70 = S x71};
                          let {x69 = S x70};
                          let {x68 = S x69};
                          let {x67 = S x68};
                          let {x66 = S x67};
                          let {x65 = S x66};
                          let {x64 = S x65};
                          let {x63 = S x64};
                          let {x62 = S x63};
                          let {x61 = S x62};
                          let {x60 = S x61};
                          let {x59 = x58};
                          let {x1 = S x59};
                          let {x76 = x60};
                          let {x0 = S x76};
                          return (x0, x1)}]
multiplysdsI x0 = msum [do {x1 <- case x0 of
                                  {S y1 -> return y1; _ -> mzero};
                            multiplysdsI x1;
                            return ()}]
multiplysdsO = msum [do {x1 <- multiplysdsO;
                         let {x0 = S x1};
                         return x0}]