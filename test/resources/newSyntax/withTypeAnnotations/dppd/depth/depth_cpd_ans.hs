module Depth_cpd_ans where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
depthI x0 gen_prog_clauseDepth3O_x16 = msum [do {depth0I x0;
                                                 x1 <- depth1O;
                                                 return ()},
                                             do {depth2I x0;
                                                 x2 <- prog_clauseDepthO gen_prog_clauseDepth3O_x16;
                                                 return ()}]
depthO gen_depth0O_x2 gen_depth2O_x8 gen_prog_clauseDepth3O_x16 = msum [do {x0 <- depth0O gen_depth0O_x2;
                                                                            x1 <- depth1O;
                                                                            return x0},
                                                                        do {x0 <- depth2O gen_depth2O_x8;
                                                                            x2 <- prog_clauseDepthO gen_prog_clauseDepth3O_x16;
                                                                            return x0}]
depth0I x0 = msum [do {x2 <- case x0 of
                             {S y2 -> return y2; _ -> mzero};
                       x3 <- case x2 of
                             {S y3 -> return y3; _ -> mzero};
                       x4 <- case x3 of
                             {S y4 -> return y4; _ -> mzero};
                       x5 <- case x4 of
                             {S y5 -> return y5; _ -> mzero};
                       x6 <- case x5 of
                             {S y6 -> return y6; _ -> mzero};
                       x7 <- case x6 of
                             {S y7 -> return y7; _ -> mzero};
                       x1 <- case x7 of
                             {S y1 -> return y1; _ -> mzero};
                       return ()}]
depth0O gen_depth0O_x2 = msum [do {(x0,
                                    x2) <- do {x2 <- gen_depth0O_x2;
                                               let {x0 = S x2};
                                               return (x0, x2)};
                                   x3 <- case x2 of
                                         {S y3 -> return y3; _ -> mzero};
                                   x4 <- case x3 of
                                         {S y4 -> return y4; _ -> mzero};
                                   x5 <- case x4 of
                                         {S y5 -> return y5; _ -> mzero};
                                   x6 <- case x5 of
                                         {S y6 -> return y6; _ -> mzero};
                                   x7 <- case x6 of
                                         {S y7 -> return y7; _ -> mzero};
                                   x1 <- case x7 of
                                         {S y1 -> return y1; _ -> mzero};
                                   return x0}]
depth1O = msum [do {let {x0 = O}; return x0}]
depth2I x0 = msum [do {x8 <- case x0 of
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
                       x2 <- case x15 of
                             {S y2 -> return y2; _ -> mzero};
                       return ()}]
depth2O gen_depth2O_x8 = msum [do {(x0,
                                    x8) <- do {x8 <- gen_depth2O_x8;
                                               let {x0 = S x8};
                                               return (x0, x8)};
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
                                   x2 <- case x15 of
                                         {S y2 -> return y2; _ -> mzero};
                                   return x0}]
prog_clauseDepthO gen_prog_clauseDepth3O_x16 = msum [do {x0 <- depth1O;
                                                         return x0},
                                                     do {x0 <- prog_clauseDepth3O gen_prog_clauseDepth3O_x16;
                                                         x1 <- prog_clauseDepth1O;
                                                         return x0}]
prog_clauseDepth1O = msum [do {x0 <- depth1O; return x0},
                           do {x1 <- prog_clauseDepth2O; let {x0 = S x1}; return x0}]
prog_clauseDepth2O = msum [do {x0 <- depth1O; return x0}]
prog_clauseDepth3O gen_prog_clauseDepth3O_x16 = msum [do {(x0,
                                                           x16) <- do {x16 <- gen_prog_clauseDepth3O_x16;
                                                                       let {x0 = S x16};
                                                                       return (x0, x16)};
                                                          x17 <- case x16 of
                                                                 {S y17 -> return y17; _ -> mzero};
                                                          x1 <- case x17 of
                                                                {S y1 -> return y1; _ -> mzero};
                                                          return x0}]