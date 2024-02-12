module MatchSimple where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
matchI x0 = msum [do {guard (x0 == Nil); return ()},
                  do {let {x3 = O};
                      let {x2 = S x3};
                      let {x1 = S x2};
                      let {x4 = Nil};
                      (x5, x6) <- case x0 of
                                  {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                      guard (x5 == x1);
                      guard (x6 == x4);
                      return ()},
                  do {let {x9 = O};
                      let {x8 = S x9};
                      let {x7 = S x8};
                      let {x12 = O};
                      let {x11 = S x12};
                      let {x13 = Nil};
                      let {x10 = Cons x11 x13};
                      (x14, x15) <- case x0 of
                                    {Cons y14 y15 -> return (y14, y15); _ -> mzero};
                      guard (x14 == x7);
                      guard (x15 == x10);
                      return ()},
                  do {let {x18 = O};
                      let {x17 = S x18};
                      let {x16 = S x17};
                      let {x21 = O};
                      let {x20 = S x21};
                      let {x24 = O};
                      let {x23 = S x24};
                      let {x25 = Nil};
                      let {x22 = Cons x23 x25};
                      let {x19 = Cons x20 x22};
                      (x26, x27) <- case x0 of
                                    {Cons y26 y27 -> return (y26, y27); _ -> mzero};
                      guard (x26 == x16);
                      guard (x27 == x19);
                      return ()},
                  do {let {x30 = O};
                      let {x29 = S x30};
                      let {x28 = S x29};
                      let {x33 = O};
                      let {x32 = S x33};
                      let {x36 = O};
                      let {x35 = S x36};
                      let {x38 = O};
                      let {x39 = Nil};
                      let {x37 = Cons x38 x39};
                      let {x34 = Cons x35 x37};
                      let {x31 = Cons x32 x34};
                      (x40, x41) <- case x0 of
                                    {Cons y40 y41 -> return (y40, y41); _ -> mzero};
                      guard (x40 == x28);
                      guard (x41 == x31);
                      return ()},
                  do {let {x44 = O};
                      let {x43 = S x44};
                      let {x42 = S x43};
                      let {x47 = O};
                      let {x46 = S x47};
                      let {x50 = O};
                      let {x49 = S x50};
                      let {x52 = O};
                      let {x54 = O};
                      let {x55 = Nil};
                      let {x53 = Cons x54 x55};
                      let {x51 = Cons x52 x53};
                      let {x48 = Cons x49 x51};
                      let {x45 = Cons x46 x48};
                      (x56, x57) <- case x0 of
                                    {Cons y56 y57 -> return (y56, y57); _ -> mzero};
                      guard (x56 == x42);
                      guard (x57 == x45);
                      return ()},
                  do {let {x60 = O};
                      let {x59 = S x60};
                      let {x58 = S x59};
                      let {x63 = O};
                      let {x62 = S x63};
                      let {x66 = O};
                      let {x65 = S x66};
                      let {x68 = O};
                      let {x70 = O};
                      let {x74 = O};
                      let {x73 = S x74};
                      let {x72 = S x73};
                      let {x75 = Nil};
                      let {x71 = Cons x72 x75};
                      let {x69 = Cons x70 x71};
                      let {x67 = Cons x68 x69};
                      let {x64 = Cons x65 x67};
                      let {x61 = Cons x62 x64};
                      (x76, x77) <- case x0 of
                                    {Cons y76 y77 -> return (y76, y77); _ -> mzero};
                      guard (x76 == x58);
                      guard (x77 == x61);
                      return ()},
                  do {let {x80 = O};
                      let {x79 = S x80};
                      let {x78 = S x79};
                      let {x83 = O};
                      let {x82 = S x83};
                      let {x86 = O};
                      let {x85 = S x86};
                      let {x88 = O};
                      let {x90 = O};
                      let {x94 = O};
                      let {x93 = S x94};
                      let {x92 = S x93};
                      let {x96 = O};
                      let {x97 = Nil};
                      let {x95 = Cons x96 x97};
                      let {x91 = Cons x92 x95};
                      let {x89 = Cons x90 x91};
                      let {x87 = Cons x88 x89};
                      let {x84 = Cons x85 x87};
                      let {x81 = Cons x82 x84};
                      (x98, x99) <- case x0 of
                                    {Cons y98 y99 -> return (y98, y99); _ -> mzero};
                      guard (x98 == x78);
                      guard (x99 == x81);
                      return ()},
                  do {let {x102 = O};
                      let {x101 = S x102};
                      let {x100 = S x101};
                      let {x105 = O};
                      let {x104 = S x105};
                      let {x108 = O};
                      let {x107 = S x108};
                      let {x110 = O};
                      let {x112 = O};
                      let {x116 = O};
                      let {x115 = S x116};
                      let {x114 = S x115};
                      let {x118 = O};
                      let {x121 = O};
                      let {x120 = S x121};
                      let {x122 = Nil};
                      let {x119 = Cons x120 x122};
                      let {x117 = Cons x118 x119};
                      let {x113 = Cons x114 x117};
                      let {x111 = Cons x112 x113};
                      let {x109 = Cons x110 x111};
                      let {x106 = Cons x107 x109};
                      let {x103 = Cons x104 x106};
                      (x123, x124) <- case x0 of
                                      {Cons y123 y124 -> return (y123, y124); _ -> mzero};
                      guard (x123 == x100);
                      guard (x124 == x103);
                      return ()}]
matchO = msum [do {let {x0 = Nil}; return x0},
               do {let {x3 = O};
                   let {x2 = S x3};
                   let {x1 = S x2};
                   let {x4 = Nil};
                   let {x5 = x1};
                   let {x6 = x4};
                   let {x0 = Cons x5 x6};
                   return x0},
               do {let {x9 = O};
                   let {x8 = S x9};
                   let {x7 = S x8};
                   let {x12 = O};
                   let {x11 = S x12};
                   let {x13 = Nil};
                   let {x10 = Cons x11 x13};
                   let {x14 = x7};
                   let {x15 = x10};
                   let {x0 = Cons x14 x15};
                   return x0},
               do {let {x18 = O};
                   let {x17 = S x18};
                   let {x16 = S x17};
                   let {x21 = O};
                   let {x20 = S x21};
                   let {x24 = O};
                   let {x23 = S x24};
                   let {x25 = Nil};
                   let {x22 = Cons x23 x25};
                   let {x19 = Cons x20 x22};
                   let {x26 = x16};
                   let {x27 = x19};
                   let {x0 = Cons x26 x27};
                   return x0},
               do {let {x30 = O};
                   let {x29 = S x30};
                   let {x28 = S x29};
                   let {x33 = O};
                   let {x32 = S x33};
                   let {x36 = O};
                   let {x35 = S x36};
                   let {x38 = O};
                   let {x39 = Nil};
                   let {x37 = Cons x38 x39};
                   let {x34 = Cons x35 x37};
                   let {x31 = Cons x32 x34};
                   let {x40 = x28};
                   let {x41 = x31};
                   let {x0 = Cons x40 x41};
                   return x0},
               do {let {x44 = O};
                   let {x43 = S x44};
                   let {x42 = S x43};
                   let {x47 = O};
                   let {x46 = S x47};
                   let {x50 = O};
                   let {x49 = S x50};
                   let {x52 = O};
                   let {x54 = O};
                   let {x55 = Nil};
                   let {x53 = Cons x54 x55};
                   let {x51 = Cons x52 x53};
                   let {x48 = Cons x49 x51};
                   let {x45 = Cons x46 x48};
                   let {x56 = x42};
                   let {x57 = x45};
                   let {x0 = Cons x56 x57};
                   return x0},
               do {let {x60 = O};
                   let {x59 = S x60};
                   let {x58 = S x59};
                   let {x63 = O};
                   let {x62 = S x63};
                   let {x66 = O};
                   let {x65 = S x66};
                   let {x68 = O};
                   let {x70 = O};
                   let {x74 = O};
                   let {x73 = S x74};
                   let {x72 = S x73};
                   let {x75 = Nil};
                   let {x71 = Cons x72 x75};
                   let {x69 = Cons x70 x71};
                   let {x67 = Cons x68 x69};
                   let {x64 = Cons x65 x67};
                   let {x61 = Cons x62 x64};
                   let {x76 = x58};
                   let {x77 = x61};
                   let {x0 = Cons x76 x77};
                   return x0},
               do {let {x80 = O};
                   let {x79 = S x80};
                   let {x78 = S x79};
                   let {x83 = O};
                   let {x82 = S x83};
                   let {x86 = O};
                   let {x85 = S x86};
                   let {x88 = O};
                   let {x90 = O};
                   let {x94 = O};
                   let {x93 = S x94};
                   let {x92 = S x93};
                   let {x96 = O};
                   let {x97 = Nil};
                   let {x95 = Cons x96 x97};
                   let {x91 = Cons x92 x95};
                   let {x89 = Cons x90 x91};
                   let {x87 = Cons x88 x89};
                   let {x84 = Cons x85 x87};
                   let {x81 = Cons x82 x84};
                   let {x98 = x78};
                   let {x99 = x81};
                   let {x0 = Cons x98 x99};
                   return x0},
               do {let {x102 = O};
                   let {x101 = S x102};
                   let {x100 = S x101};
                   let {x105 = O};
                   let {x104 = S x105};
                   let {x108 = O};
                   let {x107 = S x108};
                   let {x110 = O};
                   let {x112 = O};
                   let {x116 = O};
                   let {x115 = S x116};
                   let {x114 = S x115};
                   let {x118 = O};
                   let {x121 = O};
                   let {x120 = S x121};
                   let {x122 = Nil};
                   let {x119 = Cons x120 x122};
                   let {x117 = Cons x118 x119};
                   let {x113 = Cons x114 x117};
                   let {x111 = Cons x112 x113};
                   let {x109 = Cons x110 x111};
                   let {x106 = Cons x107 x109};
                   let {x103 = Cons x104 x106};
                   let {x123 = x100};
                   let {x124 = x103};
                   let {x0 = Cons x123 x124};
                   return x0}]