module Hanoi_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | One
    | Pair Term Term
    | Thr
    | Two
    deriving (Show, Eq)
    

checkI x0 = msum [do {check0I x0;
                      (x1, x3, x4) <- one_stepOne_stepCheckOOO;
                      return ()}]
checkO gen_check0O_x1 gen_check0O_x4 = msum [do {x0 <- check0O gen_check0O_x1 gen_check0O_x4;
                                                 (x1, x3, x4) <- one_stepOne_stepCheckOOO;
                                                 return x0}]
check0I x0 = msum [do {(x1, x4) <- case x0 of
                                   {Cons y1 y4 -> return (y1, y4); _ -> mzero};
                       x3 <- case x4 of
                             {Cons y3 y4 -> do {guard (x4 == y4); return y3}; _ -> mzero};
                       return ()}]
check0O gen_check0O_x1 gen_check0O_x4 = msum [do {(x0,
                                                   x1,
                                                   x4) <- do {x1 <- gen_check0O_x1;
                                                              x4 <- gen_check0O_x4;
                                                              let {x0 = Cons x1 x4};
                                                              return (x0, x1, x4)};
                                                  x3 <- case x4 of
                                                        {Cons y3 y4 -> do {guard (x4 == y4);
                                                                           return y3};
                                                         _ -> mzero};
                                                  return x0}]
one_stepOne_stepCheckOOO = msum [do {let {x5 = One};
                                     let {x6 = Two};
                                     let {x0 = Pair x5 x6};
                                     (x1, x4) <- one_stepOne_stepCheck1OOI x5;
                                     let {x2 = Cons x4 x5};
                                     return (x0, x1, x2)},
                                 do {let {x7 = One};
                                     let {x8 = Thr};
                                     let {x0 = Pair x7 x8};
                                     (x1, x4, x5) <- one_stepOne_stepCheck28OOO;
                                     let {x2 = Cons x4 x5};
                                     return (x0, x1, x2)}]
one_stepOne_stepCheck1OOI x2 = msum [do {(x4, x5) <- case x2 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         let {x9 = One};
                                         let {x10 = Thr};
                                         let {x0 = Pair x9 x10};
                                         x1 <- one_stepOne_stepCheck2OII x4 x5;
                                         return (x0, x1)},
                                     do {(x4, x5) <- case x2 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         let {x11 = Two};
                                         let {x12 = Thr};
                                         let {x0 = Pair x11 x12};
                                         x1 <- one_stepOne_stepCheck28OII x4 x5;
                                         return (x0, x1)},
                                     do {(x7, x8) <- case x2 of
                                                     {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                                         let {x13 = Two};
                                         let {x14 = One};
                                         let {x0 = Pair x13 x14};
                                         x1 <- one_stepOne_stepCheckOII x7 x8;
                                         return (x0, x1)}]
one_stepOne_stepCheckOII x1 x2 = msum [do {(x4, x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           guard (x5 == One);
                                           let {x6 = Two};
                                           let {x0 = Pair x5 x6};
                                           one_stepOne_stepCheck1III x1 x4 x5;
                                           return x0},
                                       do {(x4, x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           let {x7 = One};
                                           let {x8 = Thr};
                                           let {x0 = Pair x7 x8};
                                           one_stepOne_stepCheck28III x1 x4 x5;
                                           return x0}]
one_stepOne_stepCheck1III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x9, x10) <- case x0 of
                                                            {Pair y9 y10 -> return (y9, y10);
                                                             _ -> mzero};
                                               guard (x9 == One);
                                               guard (x10 == Thr);
                                               one_stepOne_stepCheck2III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x11, x12) <- case x0 of
                                                             {Pair y11 y12 -> return (y11, y12);
                                                              _ -> mzero};
                                               guard (x11 == Two);
                                               guard (x12 == Thr);
                                               one_stepOne_stepCheck28III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x13, x14) <- case x0 of
                                                             {Pair y13 y14 -> return (y13, y14);
                                                              _ -> mzero};
                                               guard (x13 == Two);
                                               guard (x14 == One);
                                               one_stepOne_stepCheckIII x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheckIII x0 x1 x2 = msum [do {(x4,
                                               x5) <- case x2 of
                                                      {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                              x6 <- case x0 of
                                                    {Pair y5 y6 -> do {guard (x5 == y5); return y6};
                                                     _ -> mzero};
                                              guard (x5 == One);
                                              guard (x6 == Two);
                                              one_stepOne_stepCheck1III x1 x4 x5;
                                              return ()},
                                          do {(x4, x5) <- case x2 of
                                                          {Cons y4 y5 -> return (y4, y5);
                                                           _ -> mzero};
                                              (x7, x8) <- case x0 of
                                                          {Pair y7 y8 -> return (y7, y8);
                                                           _ -> mzero};
                                              guard (x7 == One);
                                              guard (x8 == Thr);
                                              one_stepOne_stepCheck28III x1 x4 x5;
                                              return ()}]
one_stepOne_stepCheck2III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x15, x16) <- case x0 of
                                                             {Pair y15 y16 -> return (y15, y16);
                                                              _ -> mzero};
                                               guard (x15 == Two);
                                               guard (x16 == One);
                                               one_stepOne_stepCheck3III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x17, x18) <- case x0 of
                                                             {Pair y17 y18 -> return (y17, y18);
                                                              _ -> mzero};
                                               guard (x17 == Two);
                                               guard (x18 == Thr);
                                               one_stepOne_stepCheck27III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x19, x20) <- case x0 of
                                                             {Pair y19 y20 -> return (y19, y20);
                                                              _ -> mzero};
                                               guard (x19 == Thr);
                                               guard (x20 == One);
                                               one_stepOne_stepCheck1III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck2OII x1 x2 = msum [do {(x4, x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                            let {x15 = Two};
                                            let {x16 = One};
                                            let {x0 = Pair x15 x16};
                                            one_stepOne_stepCheck3III x1 x4 x5;
                                            return x0},
                                        do {(x4, x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                            let {x17 = Two};
                                            let {x18 = Thr};
                                            let {x0 = Pair x17 x18};
                                            one_stepOne_stepCheck27III x1 x4 x5;
                                            return x0},
                                        do {(x7, x8) <- case x2 of
                                                        {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                                            let {x19 = Thr};
                                            let {x20 = One};
                                            let {x0 = Pair x19 x20};
                                            one_stepOne_stepCheck1III x1 x7 x8;
                                            return x0}]
one_stepOne_stepCheck27III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x165, x166) <- case x0 of
                                                                {Pair y165 y166 -> return (y165,
                                                                                           y166);
                                                                 _ -> mzero};
                                                guard (x165 == One);
                                                guard (x166 == Two);
                                                one_stepOne_stepCheck21III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x167, x168) <- case x0 of
                                                                {Pair y167 y168 -> return (y167,
                                                                                           y168);
                                                                 _ -> mzero};
                                                guard (x167 == Thr);
                                                guard (x168 == Two);
                                                one_stepOne_stepCheck2III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x169, x170) <- case x0 of
                                                                {Pair y169 y170 -> return (y169,
                                                                                           y170);
                                                                 _ -> mzero};
                                                guard (x169 == Thr);
                                                guard (x170 == One);
                                                one_stepOne_stepCheck3III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck21III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x129, x130) <- case x0 of
                                                                {Pair y129 y130 -> return (y129,
                                                                                           y130);
                                                                 _ -> mzero};
                                                guard (x129 == Two);
                                                guard (x130 == One);
                                                one_stepOne_stepCheck27III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x131, x132) <- case x0 of
                                                                {Pair y131 y132 -> return (y131,
                                                                                           y132);
                                                                 _ -> mzero};
                                                guard (x131 == Thr);
                                                guard (x132 == One);
                                                one_stepOne_stepCheck19III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x133, x134) <- case x0 of
                                                                {Pair y133 y134 -> return (y133,
                                                                                           y134);
                                                                 _ -> mzero};
                                                guard (x133 == Thr);
                                                guard (x134 == Two);
                                                one_stepOne_stepCheck20III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck19III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x117, x118) <- case x0 of
                                                                {Pair y117 y118 -> return (y117,
                                                                                           y118);
                                                                 _ -> mzero};
                                                guard (x117 == One);
                                                guard (x118 == Two);
                                                one_stepOne_stepCheck20III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x119, x120) <- case x0 of
                                                                {Pair y119 y120 -> return (y119,
                                                                                           y120);
                                                                 _ -> mzero};
                                                guard (x119 == One);
                                                guard (x120 == Thr);
                                                one_stepOne_stepCheck21III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x121, x122) <- case x0 of
                                                                {Pair y121 y122 -> return (y121,
                                                                                           y122);
                                                                 _ -> mzero};
                                                guard (x121 == Thr);
                                                guard (x122 == Two);
                                                one_stepOne_stepCheck18III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck18III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x111, x112) <- case x0 of
                                                                {Pair y111 y112 -> return (y111,
                                                                                           y112);
                                                                 _ -> mzero};
                                                guard (x111 == One);
                                                guard (x112 == Thr);
                                                one_stepOne_stepCheck17III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x113, x114) <- case x0 of
                                                                {Pair y113 y114 -> return (y113,
                                                                                           y114);
                                                                 _ -> mzero};
                                                guard (x113 == Two);
                                                guard (x114 == Thr);
                                                one_stepOne_stepCheck19III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x115, x116) <- case x0 of
                                                                {Pair y115 y116 -> return (y115,
                                                                                           y116);
                                                                 _ -> mzero};
                                                guard (x115 == One);
                                                guard (x116 == Two);
                                                one_stepOne_stepCheck22III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck17III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x105, x106) <- case x0 of
                                                                {Pair y105 y106 -> return (y105,
                                                                                           y106);
                                                                 _ -> mzero};
                                                guard (x105 == Two);
                                                guard (x106 == One);
                                                one_stepOne_stepCheck16III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x107, x108) <- case x0 of
                                                                {Pair y107 y108 -> return (y107,
                                                                                           y108);
                                                                 _ -> mzero};
                                                guard (x107 == Thr);
                                                guard (x108 == One);
                                                one_stepOne_stepCheck18III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x109, x110) <- case x0 of
                                                                {Pair y109 y110 -> return (y109,
                                                                                           y110);
                                                                 _ -> mzero};
                                                guard (x109 == Thr);
                                                guard (x110 == Two);
                                                one_stepOne_stepCheck22III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck16III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x99, x100) <- case x0 of
                                                               {Pair y99 y100 -> return (y99, y100);
                                                                _ -> mzero};
                                                guard (x99 == One);
                                                guard (x100 == Two);
                                                one_stepOne_stepCheck17III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x101, x102) <- case x0 of
                                                                {Pair y101 y102 -> return (y101,
                                                                                           y102);
                                                                 _ -> mzero};
                                                guard (x101 == Thr);
                                                guard (x102 == One);
                                                one_stepOne_stepCheck15III x1 x7 x8;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x103, x104) <- case x0 of
                                                                {Pair y103 y104 -> return (y103,
                                                                                           y104);
                                                                 _ -> mzero};
                                                guard (x103 == Thr);
                                                guard (x104 == Two);
                                                one_stepOne_stepCheck23III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck15III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x93, x94) <- case x0 of
                                                              {Pair y93 y94 -> return (y93, y94);
                                                               _ -> mzero};
                                                guard (x93 == One);
                                                guard (x94 == Thr);
                                                one_stepOne_stepCheck16III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x95, x96) <- case x0 of
                                                              {Pair y95 y96 -> return (y95, y96);
                                                               _ -> mzero};
                                                guard (x95 == Two);
                                                guard (x96 == Thr);
                                                one_stepOne_stepCheck14III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x97, x98) <- case x0 of
                                                              {Pair y97 y98 -> return (y97, y98);
                                                               _ -> mzero};
                                                guard (x97 == One);
                                                guard (x98 == Two);
                                                one_stepOne_stepCheck23III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck14III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x87, x88) <- case x0 of
                                                              {Pair y87 y88 -> return (y87, y88);
                                                               _ -> mzero};
                                                guard (x87 == One);
                                                guard (x88 == Two);
                                                one_stepOne_stepCheck13III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x89, x90) <- case x0 of
                                                              {Pair y89 y90 -> return (y89, y90);
                                                               _ -> mzero};
                                                guard (x89 == Thr);
                                                guard (x90 == Two);
                                                one_stepOne_stepCheck15III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x91, x92) <- case x0 of
                                                              {Pair y91 y92 -> return (y91, y92);
                                                               _ -> mzero};
                                                guard (x91 == One);
                                                guard (x92 == Thr);
                                                one_stepOne_stepCheck24III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck13III x0 x1 x2 = msum [do {guard (x2 == Nil);
                                                (x79, x80) <- case x0 of
                                                              {Pair y79 y80 -> return (y79, y80);
                                                               _ -> mzero};
                                                guard (x79 == One);
                                                guard (x80 == Thr);
                                                one_step11I x1;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x81, x82) <- case x0 of
                                                              {Pair y81 y82 -> return (y81, y82);
                                                               _ -> mzero};
                                                guard (x81 == One);
                                                guard (x82 == Thr);
                                                one_stepOne_stepCheck12III x1 x3 x4;
                                                return ()},
                                            do {(x6, x7) <- case x2 of
                                                            {Cons y6 y7 -> return (y6, y7);
                                                             _ -> mzero};
                                                (x83, x84) <- case x0 of
                                                              {Pair y83 y84 -> return (y83, y84);
                                                               _ -> mzero};
                                                guard (x83 == Two);
                                                guard (x84 == One);
                                                one_stepOne_stepCheck14III x1 x6 x7;
                                                return ()},
                                            do {(x6, x7) <- case x2 of
                                                            {Cons y6 y7 -> return (y6, y7);
                                                             _ -> mzero};
                                                (x85, x86) <- case x0 of
                                                              {Pair y85 y86 -> return (y85, y86);
                                                               _ -> mzero};
                                                guard (x85 == Two);
                                                guard (x86 == Thr);
                                                one_stepOne_stepCheck24III x1 x6 x7;
                                                return ()}]
one_step11I x0 = msum [do {(x69, x70) <- case x0 of
                                         {Pair y69 y70 -> return (y69, y70); _ -> mzero};
                           guard (x69 == Two);
                           guard (x70 == Thr);
                           return ()}]
one_stepOne_stepCheck12III x0 x1 x2 = msum [do {guard (x2 == Nil);
                                                (x71, x72) <- case x0 of
                                                              {Pair y71 y72 -> return (y71, y72);
                                                               _ -> mzero};
                                                guard (x71 == Two);
                                                guard (x72 == One);
                                                one_step9I x1;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x73, x74) <- case x0 of
                                                              {Pair y73 y74 -> return (y73, y74);
                                                               _ -> mzero};
                                                guard (x73 == Two);
                                                guard (x74 == One);
                                                one_stepOne_stepCheck10III x1 x3 x4;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x75, x76) <- case x0 of
                                                              {Pair y75 y76 -> return (y75, y76);
                                                               _ -> mzero};
                                                guard (x75 == Thr);
                                                guard (x76 == One);
                                                one_stepOne_stepCheck13III x1 x3 x4;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x77, x78) <- case x0 of
                                                              {Pair y77 y78 -> return (y77, y78);
                                                               _ -> mzero};
                                                guard (x77 == Two);
                                                guard (x78 == Thr);
                                                one_stepOne_stepCheck25III x1 x7 x8;
                                                return ()}]
one_step9I x0 = msum [do {(x59, x60) <- case x0 of
                                        {Pair y59 y60 -> return (y59, y60); _ -> mzero};
                          guard (x59 == One);
                          guard (x60 == Thr);
                          return ()}]
one_stepOne_stepCheck10III x0 x1 x2 = msum [do {guard (x2 == Nil);
                                                (x61, x62) <- case x0 of
                                                              {Pair y61 y62 -> return (y61, y62);
                                                               _ -> mzero};
                                                guard (x61 == One);
                                                guard (x62 == Two);
                                                one_step11I x1;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x63, x64) <- case x0 of
                                                              {Pair y63 y64 -> return (y63, y64);
                                                               _ -> mzero};
                                                guard (x63 == One);
                                                guard (x64 == Two);
                                                one_stepOne_stepCheck12III x1 x3 x4;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x65, x66) <- case x0 of
                                                              {Pair y65 y66 -> return (y65, y66);
                                                               _ -> mzero};
                                                guard (x65 == Thr);
                                                guard (x66 == Two);
                                                one_stepOne_stepCheck8III x1 x3 x4;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x67, x68) <- case x0 of
                                                              {Pair y67 y68 -> return (y67, y68);
                                                               _ -> mzero};
                                                guard (x67 == One);
                                                guard (x68 == Thr);
                                                one_stepOne_stepCheck25III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck20III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x123, x124) <- case x0 of
                                                                {Pair y123 y124 -> return (y123,
                                                                                           y124);
                                                                 _ -> mzero};
                                                guard (x123 == Two);
                                                guard (x124 == One);
                                                one_stepOne_stepCheck19III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x125, x126) <- case x0 of
                                                                {Pair y125 y126 -> return (y125,
                                                                                           y126);
                                                                 _ -> mzero};
                                                guard (x125 == Thr);
                                                guard (x126 == One);
                                                one_stepOne_stepCheck23III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x127, x128) <- case x0 of
                                                                {Pair y127 y128 -> return (y127,
                                                                                           y128);
                                                                 _ -> mzero};
                                                guard (x127 == Two);
                                                guard (x128 == Thr);
                                                one_stepOne_stepCheck21III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck22III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x135, x136) <- case x0 of
                                                                {Pair y135 y136 -> return (y135,
                                                                                           y136);
                                                                 _ -> mzero};
                                                guard (x135 == Two);
                                                guard (x136 == One);
                                                one_stepOne_stepCheck18III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x137, x138) <- case x0 of
                                                                {Pair y137 y138 -> return (y137,
                                                                                           y138);
                                                                 _ -> mzero};
                                                guard (x137 == Two);
                                                guard (x138 == Thr);
                                                one_stepOne_stepCheck17III x1 x4 x5;
                                                return ()}]
one_stepOne_stepCheck23III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x139, x140) <- case x0 of
                                                                {Pair y139 y140 -> return (y139,
                                                                                           y140);
                                                                 _ -> mzero};
                                                guard (x139 == One);
                                                guard (x140 == Thr);
                                                one_stepOne_stepCheck20III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x141, x142) <- case x0 of
                                                                {Pair y141 y142 -> return (y141,
                                                                                           y142);
                                                                 _ -> mzero};
                                                guard (x141 == Two);
                                                guard (x142 == Thr);
                                                one_stepOne_stepCheck16III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x143, x144) <- case x0 of
                                                                {Pair y143 y144 -> return (y143,
                                                                                           y144);
                                                                 _ -> mzero};
                                                guard (x143 == Two);
                                                guard (x144 == One);
                                                one_stepOne_stepCheck15III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck24III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x145, x146) <- case x0 of
                                                                {Pair y145 y146 -> return (y145,
                                                                                           y146);
                                                                 _ -> mzero};
                                                guard (x145 == One);
                                                guard (x146 == Two);
                                                one_stepOne_stepCheck26III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x147, x148) <- case x0 of
                                                                {Pair y147 y148 -> return (y147,
                                                                                           y148);
                                                                 _ -> mzero};
                                                guard (x147 == Thr);
                                                guard (x148 == Two);
                                                one_stepOne_stepCheck13III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x149, x150) <- case x0 of
                                                                {Pair y149 y150 -> return (y149,
                                                                                           y150);
                                                                 _ -> mzero};
                                                guard (x149 == Thr);
                                                guard (x150 == One);
                                                one_stepOne_stepCheck14III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck25III x0 x1 x2 = msum [do {guard (x2 == Nil);
                                                (x151, x152) <- case x0 of
                                                                {Pair y151 y152 -> return (y151,
                                                                                           y152);
                                                                 _ -> mzero};
                                                guard (x151 == Thr);
                                                guard (x152 == One);
                                                one_step9I x1;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x153, x154) <- case x0 of
                                                                {Pair y153 y154 -> return (y153,
                                                                                           y154);
                                                                 _ -> mzero};
                                                guard (x153 == Thr);
                                                guard (x154 == One);
                                                one_stepOne_stepCheck10III x1 x3 x4;
                                                return ()},
                                            do {guard (x2 == Nil);
                                                (x155, x156) <- case x0 of
                                                                {Pair y155 y156 -> return (y155,
                                                                                           y156);
                                                                 _ -> mzero};
                                                guard (x155 == Thr);
                                                guard (x156 == Two);
                                                one_step11I x1;
                                                return ()},
                                            do {(x3, x4) <- case x2 of
                                                            {Cons y3 y4 -> return (y3, y4);
                                                             _ -> mzero};
                                                (x157, x158) <- case x0 of
                                                                {Pair y157 y158 -> return (y157,
                                                                                           y158);
                                                                 _ -> mzero};
                                                guard (x157 == Thr);
                                                guard (x158 == Two);
                                                one_stepOne_stepCheck12III x1 x3 x4;
                                                return ()}]
one_stepOne_stepCheck26III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x159, x160) <- case x0 of
                                                                {Pair y159 y160 -> return (y159,
                                                                                           y160);
                                                                 _ -> mzero};
                                                guard (x159 == Two);
                                                guard (x160 == One);
                                                one_stepOne_stepCheck24III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x161, x162) <- case x0 of
                                                                {Pair y161 y162 -> return (y161,
                                                                                           y162);
                                                                 _ -> mzero};
                                                guard (x161 == Thr);
                                                guard (x162 == One);
                                                one_stepOne_stepCheck8III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x163, x164) <- case x0 of
                                                                {Pair y163 y164 -> return (y163,
                                                                                           y164);
                                                                 _ -> mzero};
                                                guard (x163 == Thr);
                                                guard (x164 == Two);
                                                one_stepOne_stepCheck7III x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck28III x0 x1 x2 = msum [do {(x4,
                                                 x5) <- case x2 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                                (x171, x172) <- case x0 of
                                                                {Pair y171 y172 -> return (y171,
                                                                                           y172);
                                                                 _ -> mzero};
                                                guard (x171 == One);
                                                guard (x172 == Two);
                                                one_stepOne_stepCheck5III x1 x4 x5;
                                                return ()},
                                            do {(x4, x5) <- case x2 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                (x173, x174) <- case x0 of
                                                                {Pair y173 y174 -> return (y173,
                                                                                           y174);
                                                                 _ -> mzero};
                                                guard (x173 == Thr);
                                                guard (x174 == Two);
                                                one_stepOne_stepCheck1III x1 x4 x5;
                                                return ()},
                                            do {(x7, x8) <- case x2 of
                                                            {Cons y7 y8 -> return (y7, y8);
                                                             _ -> mzero};
                                                (x175, x176) <- case x0 of
                                                                {Pair y175 y176 -> return (y175,
                                                                                           y176);
                                                                 _ -> mzero};
                                                guard (x175 == Thr);
                                                guard (x176 == One);
                                                one_stepOne_stepCheckIII x1 x7 x8;
                                                return ()}]
one_stepOne_stepCheck28OII x1 x2 = msum [do {(x4, x5) <- case x2 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             let {x171 = One};
                                             let {x172 = Two};
                                             let {x0 = Pair x171 x172};
                                             one_stepOne_stepCheck5III x1 x4 x5;
                                             return x0},
                                         do {(x4, x5) <- case x2 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             let {x173 = Thr};
                                             let {x174 = Two};
                                             let {x0 = Pair x173 x174};
                                             one_stepOne_stepCheck1III x1 x4 x5;
                                             return x0},
                                         do {(x7, x8) <- case x2 of
                                                         {Cons y7 y8 -> return (y7, y8);
                                                          _ -> mzero};
                                             let {x175 = Thr};
                                             let {x176 = One};
                                             let {x0 = Pair x175 x176};
                                             one_stepOne_stepCheckIII x1 x7 x8;
                                             return x0}]
one_stepOne_stepCheck28OOO = msum [do {let {x171 = One};
                                       let {x172 = Two};
                                       let {x0 = Pair x171 x172};
                                       (x1, x4, x5) <- one_stepOne_stepCheck5OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x173 = Thr};
                                       let {x174 = Two};
                                       let {x0 = Pair x173 x174};
                                       (x1, x4, x5) <- one_stepOne_stepCheck1OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x175 = Thr};
                                       let {x176 = One};
                                       let {x0 = Pair x175 x176};
                                       (x1, x7, x8) <- one_stepOne_stepCheckOOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck1OOO = msum [do {let {x9 = One};
                                      let {x10 = Thr};
                                      let {x0 = Pair x9 x10};
                                      (x1, x4, x5) <- one_stepOne_stepCheck2OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x11 = Two};
                                      let {x12 = Thr};
                                      let {x0 = Pair x11 x12};
                                      (x1, x4, x5) <- one_stepOne_stepCheck28OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x13 = Two};
                                      let {x14 = One};
                                      let {x0 = Pair x13 x14};
                                      (x1, x7, x8) <- one_stepOne_stepCheckOOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck2OOO = msum [do {let {x15 = Two};
                                      let {x16 = One};
                                      let {x0 = Pair x15 x16};
                                      (x1, x4, x5) <- one_stepOne_stepCheck3OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x17 = Two};
                                      let {x18 = Thr};
                                      let {x0 = Pair x17 x18};
                                      (x1, x4, x5) <- one_stepOne_stepCheck27OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x19 = Thr};
                                      let {x20 = One};
                                      let {x0 = Pair x19 x20};
                                      (x1, x7, x8) <- one_stepOne_stepCheck1OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck27OOO = msum [do {let {x165 = One};
                                       let {x166 = Two};
                                       let {x0 = Pair x165 x166};
                                       (x1, x4, x5) <- one_stepOne_stepCheck21OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x167 = Thr};
                                       let {x168 = Two};
                                       let {x0 = Pair x167 x168};
                                       (x1, x4, x5) <- one_stepOne_stepCheck2OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x169 = Thr};
                                       let {x170 = One};
                                       let {x0 = Pair x169 x170};
                                       (x1, x7, x8) <- one_stepOne_stepCheck3OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck21OOO = msum [do {let {x129 = Two};
                                       let {x130 = One};
                                       let {x0 = Pair x129 x130};
                                       (x1, x4, x5) <- one_stepOne_stepCheck27OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x131 = Thr};
                                       let {x132 = One};
                                       let {x0 = Pair x131 x132};
                                       (x1, x4, x5) <- one_stepOne_stepCheck19OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x133 = Thr};
                                       let {x134 = Two};
                                       let {x0 = Pair x133 x134};
                                       (x1, x7, x8) <- one_stepOne_stepCheck20OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck19OOO = msum [do {let {x117 = One};
                                       let {x118 = Two};
                                       let {x0 = Pair x117 x118};
                                       (x1, x4, x5) <- one_stepOne_stepCheck20OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x119 = One};
                                       let {x120 = Thr};
                                       let {x0 = Pair x119 x120};
                                       (x1, x4, x5) <- one_stepOne_stepCheck21OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x121 = Thr};
                                       let {x122 = Two};
                                       let {x0 = Pair x121 x122};
                                       (x1, x7, x8) <- one_stepOne_stepCheck18OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck18OOO = msum [do {let {x111 = One};
                                       let {x112 = Thr};
                                       let {x0 = Pair x111 x112};
                                       (x1, x4, x5) <- one_stepOne_stepCheck17OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x113 = Two};
                                       let {x114 = Thr};
                                       let {x0 = Pair x113 x114};
                                       (x1, x4, x5) <- one_stepOne_stepCheck19OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x115 = One};
                                       let {x116 = Two};
                                       let {x0 = Pair x115 x116};
                                       (x1, x7, x8) <- one_stepOne_stepCheck22OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck17OOO = msum [do {let {x105 = Two};
                                       let {x106 = One};
                                       let {x0 = Pair x105 x106};
                                       (x1, x4, x5) <- one_stepOne_stepCheck16OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x107 = Thr};
                                       let {x108 = One};
                                       let {x0 = Pair x107 x108};
                                       (x1, x4, x5) <- one_stepOne_stepCheck18OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x109 = Thr};
                                       let {x110 = Two};
                                       let {x0 = Pair x109 x110};
                                       (x1, x7, x8) <- one_stepOne_stepCheck22OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck16OOO = msum [do {let {x99 = One};
                                       let {x100 = Two};
                                       let {x0 = Pair x99 x100};
                                       (x1, x4, x5) <- one_stepOne_stepCheck17OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x101 = Thr};
                                       let {x102 = One};
                                       let {x0 = Pair x101 x102};
                                       (x1, x7, x8) <- one_stepOne_stepCheck15OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)},
                                   do {let {x103 = Thr};
                                       let {x104 = Two};
                                       let {x0 = Pair x103 x104};
                                       (x1, x7, x8) <- one_stepOne_stepCheck23OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck15OOO = msum [do {let {x93 = One};
                                       let {x94 = Thr};
                                       let {x0 = Pair x93 x94};
                                       (x1, x4, x5) <- one_stepOne_stepCheck16OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x95 = Two};
                                       let {x96 = Thr};
                                       let {x0 = Pair x95 x96};
                                       (x1, x4, x5) <- one_stepOne_stepCheck14OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x97 = One};
                                       let {x98 = Two};
                                       let {x0 = Pair x97 x98};
                                       (x1, x7, x8) <- one_stepOne_stepCheck23OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck14OOO = msum [do {let {x87 = One};
                                       let {x88 = Two};
                                       let {x0 = Pair x87 x88};
                                       (x1, x4, x5) <- one_stepOne_stepCheck13OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x89 = Thr};
                                       let {x90 = Two};
                                       let {x0 = Pair x89 x90};
                                       (x1, x4, x5) <- one_stepOne_stepCheck15OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x91 = One};
                                       let {x92 = Thr};
                                       let {x0 = Pair x91 x92};
                                       (x1, x7, x8) <- one_stepOne_stepCheck24OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck13OOO = msum [do {let {x2 = Nil};
                                       let {x79 = One};
                                       let {x80 = Thr};
                                       let {x0 = Pair x79 x80};
                                       x1 <- one_step11O;
                                       return (x0, x1, x2)},
                                   do {let {x81 = One};
                                       let {x82 = Thr};
                                       let {x0 = Pair x81 x82};
                                       (x1, x3, x4) <- one_stepOne_stepCheck12OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x83 = Two};
                                       let {x84 = One};
                                       let {x0 = Pair x83 x84};
                                       (x1, x6, x7) <- one_stepOne_stepCheck14OOO;
                                       let {x2 = Cons x6 x7};
                                       return (x0, x1, x2)},
                                   do {let {x85 = Two};
                                       let {x86 = Thr};
                                       let {x0 = Pair x85 x86};
                                       (x1, x6, x7) <- one_stepOne_stepCheck24OOO;
                                       let {x2 = Cons x6 x7};
                                       return (x0, x1, x2)}]
one_step11O = msum [do {let {x69 = Two};
                        let {x70 = Thr};
                        let {x0 = Pair x69 x70};
                        return x0}]
one_stepOne_stepCheck12OOO = msum [do {let {x2 = Nil};
                                       let {x71 = Two};
                                       let {x72 = One};
                                       let {x0 = Pair x71 x72};
                                       x1 <- one_step9O;
                                       return (x0, x1, x2)},
                                   do {let {x73 = Two};
                                       let {x74 = One};
                                       let {x0 = Pair x73 x74};
                                       (x1, x3, x4) <- one_stepOne_stepCheck10OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x75 = Thr};
                                       let {x76 = One};
                                       let {x0 = Pair x75 x76};
                                       (x1, x3, x4) <- one_stepOne_stepCheck13OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x77 = Two};
                                       let {x78 = Thr};
                                       let {x0 = Pair x77 x78};
                                       (x1, x7, x8) <- one_stepOne_stepCheck25OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_step9O = msum [do {let {x59 = One};
                       let {x60 = Thr};
                       let {x0 = Pair x59 x60};
                       return x0}]
one_stepOne_stepCheck10OOO = msum [do {let {x2 = Nil};
                                       let {x61 = One};
                                       let {x62 = Two};
                                       let {x0 = Pair x61 x62};
                                       x1 <- one_step11O;
                                       return (x0, x1, x2)},
                                   do {let {x63 = One};
                                       let {x64 = Two};
                                       let {x0 = Pair x63 x64};
                                       (x1, x3, x4) <- one_stepOne_stepCheck12OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x65 = Thr};
                                       let {x66 = Two};
                                       let {x0 = Pair x65 x66};
                                       (x1, x3, x4) <- one_stepOne_stepCheck8OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x67 = One};
                                       let {x68 = Thr};
                                       let {x0 = Pair x67 x68};
                                       (x1, x7, x8) <- one_stepOne_stepCheck25OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck20OOO = msum [do {let {x123 = Two};
                                       let {x124 = One};
                                       let {x0 = Pair x123 x124};
                                       (x1, x4, x5) <- one_stepOne_stepCheck19OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x125 = Thr};
                                       let {x126 = One};
                                       let {x0 = Pair x125 x126};
                                       (x1, x4, x5) <- one_stepOne_stepCheck23OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x127 = Two};
                                       let {x128 = Thr};
                                       let {x0 = Pair x127 x128};
                                       (x1, x7, x8) <- one_stepOne_stepCheck21OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck22OOO = msum [do {let {x135 = Two};
                                       let {x136 = One};
                                       let {x0 = Pair x135 x136};
                                       (x1, x4, x5) <- one_stepOne_stepCheck18OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x137 = Two};
                                       let {x138 = Thr};
                                       let {x0 = Pair x137 x138};
                                       (x1, x4, x5) <- one_stepOne_stepCheck17OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck23OOO = msum [do {let {x139 = One};
                                       let {x140 = Thr};
                                       let {x0 = Pair x139 x140};
                                       (x1, x4, x5) <- one_stepOne_stepCheck20OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x141 = Two};
                                       let {x142 = Thr};
                                       let {x0 = Pair x141 x142};
                                       (x1, x4, x5) <- one_stepOne_stepCheck16OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x143 = Two};
                                       let {x144 = One};
                                       let {x0 = Pair x143 x144};
                                       (x1, x7, x8) <- one_stepOne_stepCheck15OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck24OOO = msum [do {let {x145 = One};
                                       let {x146 = Two};
                                       let {x0 = Pair x145 x146};
                                       (x1, x4, x5) <- one_stepOne_stepCheck26OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x147 = Thr};
                                       let {x148 = Two};
                                       let {x0 = Pair x147 x148};
                                       (x1, x4, x5) <- one_stepOne_stepCheck13OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x149 = Thr};
                                       let {x150 = One};
                                       let {x0 = Pair x149 x150};
                                       (x1, x7, x8) <- one_stepOne_stepCheck14OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck25OOO = msum [do {let {x2 = Nil};
                                       let {x151 = Thr};
                                       let {x152 = One};
                                       let {x0 = Pair x151 x152};
                                       x1 <- one_step9O;
                                       return (x0, x1, x2)},
                                   do {let {x153 = Thr};
                                       let {x154 = One};
                                       let {x0 = Pair x153 x154};
                                       (x1, x3, x4) <- one_stepOne_stepCheck10OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)},
                                   do {let {x2 = Nil};
                                       let {x155 = Thr};
                                       let {x156 = Two};
                                       let {x0 = Pair x155 x156};
                                       x1 <- one_step11O;
                                       return (x0, x1, x2)},
                                   do {let {x157 = Thr};
                                       let {x158 = Two};
                                       let {x0 = Pair x157 x158};
                                       (x1, x3, x4) <- one_stepOne_stepCheck12OOO;
                                       let {x2 = Cons x3 x4};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck26OOO = msum [do {let {x159 = Two};
                                       let {x160 = One};
                                       let {x0 = Pair x159 x160};
                                       (x1, x4, x5) <- one_stepOne_stepCheck24OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x161 = Thr};
                                       let {x162 = One};
                                       let {x0 = Pair x161 x162};
                                       (x1, x4, x5) <- one_stepOne_stepCheck8OOO;
                                       let {x2 = Cons x4 x5};
                                       return (x0, x1, x2)},
                                   do {let {x163 = Thr};
                                       let {x164 = Two};
                                       let {x0 = Pair x163 x164};
                                       (x1, x7, x8) <- one_stepOne_stepCheck7OOO;
                                       let {x2 = Cons x7 x8};
                                       return (x0, x1, x2)}]
one_stepOne_stepCheck3III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x21, x22) <- case x0 of
                                                             {Pair y21 y22 -> return (y21, y22);
                                                              _ -> mzero};
                                               guard (x21 == One);
                                               guard (x22 == Two);
                                               one_stepOne_stepCheck2III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x23, x24) <- case x0 of
                                                             {Pair y23 y24 -> return (y23, y24);
                                                              _ -> mzero};
                                               guard (x23 == Thr);
                                               guard (x24 == Two);
                                               one_stepOne_stepCheck4III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x25, x26) <- case x0 of
                                                             {Pair y25 y26 -> return (y25, y26);
                                                              _ -> mzero};
                                               guard (x25 == One);
                                               guard (x26 == Thr);
                                               one_stepOne_stepCheck27III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck3OOO = msum [do {let {x21 = One};
                                      let {x22 = Two};
                                      let {x0 = Pair x21 x22};
                                      (x1, x4, x5) <- one_stepOne_stepCheck2OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x23 = Thr};
                                      let {x24 = Two};
                                      let {x0 = Pair x23 x24};
                                      (x1, x4, x5) <- one_stepOne_stepCheck4OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x25 = One};
                                      let {x26 = Thr};
                                      let {x0 = Pair x25 x26};
                                      (x1, x7, x8) <- one_stepOne_stepCheck27OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck4III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x27, x28) <- case x0 of
                                                             {Pair y27 y28 -> return (y27, y28);
                                                              _ -> mzero};
                                               guard (x27 == One);
                                               guard (x28 == Thr);
                                               one_stepOne_stepCheck5III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x29, x30) <- case x0 of
                                                             {Pair y29 y30 -> return (y29, y30);
                                                              _ -> mzero};
                                               guard (x29 == Two);
                                               guard (x30 == Thr);
                                               one_stepOne_stepCheck3III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x31, x32) <- case x0 of
                                                             {Pair y31 y32 -> return (y31, y32);
                                                              _ -> mzero};
                                               guard (x31 == One);
                                               guard (x32 == Two);
                                               one_stepOne_stepCheck6III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck4OOO = msum [do {let {x27 = One};
                                      let {x28 = Thr};
                                      let {x0 = Pair x27 x28};
                                      (x1, x4, x5) <- one_stepOne_stepCheck5OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x29 = Two};
                                      let {x30 = Thr};
                                      let {x0 = Pair x29 x30};
                                      (x1, x4, x5) <- one_stepOne_stepCheck3OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x31 = One};
                                      let {x32 = Two};
                                      let {x0 = Pair x31 x32};
                                      (x1, x7, x8) <- one_stepOne_stepCheck6OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck5III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x33, x34) <- case x0 of
                                                             {Pair y33 y34 -> return (y33, y34);
                                                              _ -> mzero};
                                               guard (x33 == Two);
                                               guard (x34 == One);
                                               one_stepOne_stepCheck28III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x35, x36) <- case x0 of
                                                             {Pair y35 y36 -> return (y35, y36);
                                                              _ -> mzero};
                                               guard (x35 == Thr);
                                               guard (x36 == One);
                                               one_stepOne_stepCheck4III x1 x7 x8;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x37, x38) <- case x0 of
                                                             {Pair y37 y38 -> return (y37, y38);
                                                              _ -> mzero};
                                               guard (x37 == Thr);
                                               guard (x38 == Two);
                                               one_stepOne_stepCheck6III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck5OOO = msum [do {let {x33 = Two};
                                      let {x34 = One};
                                      let {x0 = Pair x33 x34};
                                      (x1, x4, x5) <- one_stepOne_stepCheck28OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x35 = Thr};
                                      let {x36 = One};
                                      let {x0 = Pair x35 x36};
                                      (x1, x7, x8) <- one_stepOne_stepCheck4OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)},
                                  do {let {x37 = Thr};
                                      let {x38 = Two};
                                      let {x0 = Pair x37 x38};
                                      (x1, x7, x8) <- one_stepOne_stepCheck6OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck6III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x39, x40) <- case x0 of
                                                             {Pair y39 y40 -> return (y39, y40);
                                                              _ -> mzero};
                                               guard (x39 == One);
                                               guard (x40 == Thr);
                                               one_stepOne_stepCheck7III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x41, x42) <- case x0 of
                                                             {Pair y41 y42 -> return (y41, y42);
                                                              _ -> mzero};
                                               guard (x41 == Two);
                                               guard (x42 == Thr);
                                               one_stepOne_stepCheck5III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x43, x44) <- case x0 of
                                                             {Pair y43 y44 -> return (y43, y44);
                                                              _ -> mzero};
                                               guard (x43 == Two);
                                               guard (x44 == One);
                                               one_stepOne_stepCheck4III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck6OOO = msum [do {let {x39 = One};
                                      let {x40 = Thr};
                                      let {x0 = Pair x39 x40};
                                      (x1, x4, x5) <- one_stepOne_stepCheck7OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x41 = Two};
                                      let {x42 = Thr};
                                      let {x0 = Pair x41 x42};
                                      (x1, x4, x5) <- one_stepOne_stepCheck5OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x43 = Two};
                                      let {x44 = One};
                                      let {x0 = Pair x43 x44};
                                      (x1, x7, x8) <- one_stepOne_stepCheck4OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck7III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x45, x46) <- case x0 of
                                                             {Pair y45 y46 -> return (y45, y46);
                                                              _ -> mzero};
                                               guard (x45 == Two);
                                               guard (x46 == One);
                                               one_stepOne_stepCheck8III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x47, x48) <- case x0 of
                                                             {Pair y47 y48 -> return (y47, y48);
                                                              _ -> mzero};
                                               guard (x47 == Thr);
                                               guard (x48 == One);
                                               one_stepOne_stepCheck6III x1 x4 x5;
                                               return ()},
                                           do {(x7, x8) <- case x2 of
                                                           {Cons y7 y8 -> return (y7, y8);
                                                            _ -> mzero};
                                               (x49, x50) <- case x0 of
                                                             {Pair y49 y50 -> return (y49, y50);
                                                              _ -> mzero};
                                               guard (x49 == Two);
                                               guard (x50 == Thr);
                                               one_stepOne_stepCheck26III x1 x7 x8;
                                               return ()}]
one_stepOne_stepCheck7OOO = msum [do {let {x45 = Two};
                                      let {x46 = One};
                                      let {x0 = Pair x45 x46};
                                      (x1, x4, x5) <- one_stepOne_stepCheck8OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x47 = Thr};
                                      let {x48 = One};
                                      let {x0 = Pair x47 x48};
                                      (x1, x4, x5) <- one_stepOne_stepCheck6OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x49 = Two};
                                      let {x50 = Thr};
                                      let {x0 = Pair x49 x50};
                                      (x1, x7, x8) <- one_stepOne_stepCheck26OOO;
                                      let {x2 = Cons x7 x8};
                                      return (x0, x1, x2)}]
one_stepOne_stepCheck8III x0 x1 x2 = msum [do {(x4,
                                                x5) <- case x2 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                               (x51, x52) <- case x0 of
                                                             {Pair y51 y52 -> return (y51, y52);
                                                              _ -> mzero};
                                               guard (x51 == One);
                                               guard (x52 == Two);
                                               one_stepOne_stepCheck7III x1 x4 x5;
                                               return ()},
                                           do {(x4, x5) <- case x2 of
                                                           {Cons y4 y5 -> return (y4, y5);
                                                            _ -> mzero};
                                               (x53, x54) <- case x0 of
                                                             {Pair y53 y54 -> return (y53, y54);
                                                              _ -> mzero};
                                               guard (x53 == One);
                                               guard (x54 == Thr);
                                               one_stepOne_stepCheck26III x1 x4 x5;
                                               return ()},
                                           do {guard (x2 == Nil);
                                               (x55, x56) <- case x0 of
                                                             {Pair y55 y56 -> return (y55, y56);
                                                              _ -> mzero};
                                               guard (x55 == Two);
                                               guard (x56 == Thr);
                                               one_step9I x1;
                                               return ()},
                                           do {(x6, x7) <- case x2 of
                                                           {Cons y6 y7 -> return (y6, y7);
                                                            _ -> mzero};
                                               (x57, x58) <- case x0 of
                                                             {Pair y57 y58 -> return (y57, y58);
                                                              _ -> mzero};
                                               guard (x57 == Two);
                                               guard (x58 == Thr);
                                               one_stepOne_stepCheck10III x1 x6 x7;
                                               return ()}]
one_stepOne_stepCheck8OOO = msum [do {let {x51 = One};
                                      let {x52 = Two};
                                      let {x0 = Pair x51 x52};
                                      (x1, x4, x5) <- one_stepOne_stepCheck7OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x53 = One};
                                      let {x54 = Thr};
                                      let {x0 = Pair x53 x54};
                                      (x1, x4, x5) <- one_stepOne_stepCheck26OOO;
                                      let {x2 = Cons x4 x5};
                                      return (x0, x1, x2)},
                                  do {let {x2 = Nil};
                                      let {x55 = Two};
                                      let {x56 = Thr};
                                      let {x0 = Pair x55 x56};
                                      x1 <- one_step9O;
                                      return (x0, x1, x2)},
                                  do {let {x57 = Two};
                                      let {x58 = Thr};
                                      let {x0 = Pair x57 x58};
                                      (x1, x6, x7) <- one_stepOne_stepCheck10OOO;
                                      let {x2 = Cons x6 x7};
                                      return (x0, x1, x2)}]