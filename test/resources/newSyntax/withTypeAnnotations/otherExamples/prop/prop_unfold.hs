module Prop where

import Stream
import Control.Monad

data Term
    = Conj Term Term
    | Disj Term Term
    | Falso
    | Impl Term Term
    | Lit Term
    | Neg Term
    | Trueo
    deriving (Show, Eq)
evaloI x0 = msum [do {evaloEvaloI x0; return ()},
                  do {__evaloEvaloI x0; return ()},
                  do {___evaloEvaloI x0; return ()}]
___evaloEvaloI x0 = msum [do {x1 <- case x0 of
                                    {Neg y1 -> return y1; _ -> mzero};
                              __evaloEvaloI x1;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                              ___evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                              _evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                              ___evaloEvaloI x1;
                              _evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                              ___evaloEvaloI x1;
                              evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                              evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                              ___evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                              __evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                              __evaloEvaloI x1;
                              _evaloEvaloI x2;
                              return ()},
                          do {(x1, x2) <- case x0 of
                                          {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                              evaloEvaloI x1;
                              ___evaloEvaloI x2;
                              return ()}]
__evaloEvaloI x0 = msum [do {x1 <- case x0 of
                                   {Neg y1 -> return y1; _ -> mzero};
                             ___evaloEvaloI x1;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                             __evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                             _evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                             __evaloEvaloI x1;
                             _evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                             __evaloEvaloI x1;
                             evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                             evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                             __evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                             ___evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                             ___evaloEvaloI x1;
                             _evaloEvaloI x2;
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                             evaloEvaloI x1;
                             __evaloEvaloI x2;
                             return ()}]
_evaloEvaloI x0 = msum [do {let {x5 = Falso};
                            x6 <- case x0 of
                                  {Lit y6 -> return y6; _ -> mzero};
                            guard (x6 == x5);
                            return ()},
                        do {x1 <- case x0 of
                                  {Neg y1 -> return y1; _ -> mzero};
                            evaloEvaloI x1;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                            _evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            _evaloEvaloI x1;
                            evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            __evaloEvaloI x1;
                            ___evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            _evaloEvaloI x1;
                            ___evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            ___evaloEvaloI x1;
                            __evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            ___evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            _evaloEvaloI x1;
                            __evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            __evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                            _evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()},
                        do {(x1, x2) <- case x0 of
                                        {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                            evaloEvaloI x1;
                            _evaloEvaloI x2;
                            return ()}]
evaloO = msum [do {x0 <- evaloEvaloO; return x0},
               do {x0 <- __evaloEvaloO; return x0},
               do {x0 <- ___evaloEvaloO; return x0}]
___evaloEvaloO = msum [do {x1 <- __evaloEvaloO;
                           let {x0 = Neg x1};
                           return x0},
                       do {x1 <- ___evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Disj x1 x2};
                           return x0},
                       do {x1 <- _evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Disj x1 x2};
                           return x0},
                       do {x1 <- ___evaloEvaloO;
                           x2 <- _evaloEvaloO;
                           let {x0 = Disj x1 x2};
                           return x0},
                       do {x1 <- ___evaloEvaloO;
                           x2 <- evaloEvaloO;
                           let {x0 = Conj x1 x2};
                           return x0},
                       do {x1 <- evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Conj x1 x2};
                           return x0},
                       do {x1 <- ___evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Conj x1 x2};
                           return x0},
                       do {x1 <- __evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Impl x1 x2};
                           return x0},
                       do {x1 <- __evaloEvaloO;
                           x2 <- _evaloEvaloO;
                           let {x0 = Impl x1 x2};
                           return x0},
                       do {x1 <- evaloEvaloO;
                           x2 <- ___evaloEvaloO;
                           let {x0 = Impl x1 x2};
                           return x0}]
__evaloEvaloO = msum [do {x1 <- ___evaloEvaloO;
                          let {x0 = Neg x1};
                          return x0},
                      do {x1 <- __evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Disj x1 x2};
                          return x0},
                      do {x1 <- _evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Disj x1 x2};
                          return x0},
                      do {x1 <- __evaloEvaloO;
                          x2 <- _evaloEvaloO;
                          let {x0 = Disj x1 x2};
                          return x0},
                      do {x1 <- __evaloEvaloO;
                          x2 <- evaloEvaloO;
                          let {x0 = Conj x1 x2};
                          return x0},
                      do {x1 <- evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Conj x1 x2};
                          return x0},
                      do {x1 <- __evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Conj x1 x2};
                          return x0},
                      do {x1 <- ___evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Impl x1 x2};
                          return x0},
                      do {x1 <- ___evaloEvaloO;
                          x2 <- _evaloEvaloO;
                          let {x0 = Impl x1 x2};
                          return x0},
                      do {x1 <- evaloEvaloO;
                          x2 <- __evaloEvaloO;
                          let {x0 = Impl x1 x2};
                          return x0}]
_evaloEvaloO = msum [do {let {x5 = Falso};
                         let {x6 = x5};
                         let {x0 = Lit x6};
                         return x0},
                     do {x1 <- evaloEvaloO; let {x0 = Neg x1}; return x0},
                     do {x1 <- _evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Disj x1 x2};
                         return x0},
                     do {x1 <- _evaloEvaloO;
                         x2 <- evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- __evaloEvaloO;
                         x2 <- ___evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- _evaloEvaloO;
                         x2 <- ___evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- ___evaloEvaloO;
                         x2 <- __evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- ___evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- _evaloEvaloO;
                         x2 <- __evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- __evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- _evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Conj x1 x2};
                         return x0},
                     do {x1 <- evaloEvaloO;
                         x2 <- _evaloEvaloO;
                         let {x0 = Impl x1 x2};
                         return x0}]
evaloEvaloI x0 = msum [do {let {x3 = Trueo};
                           x4 <- case x0 of
                                 {Lit y4 -> return y4; _ -> mzero};
                           guard (x4 == x3);
                           return ()},
                       do {x1 <- case x0 of
                                 {Neg y1 -> return y1; _ -> mzero};
                           _evaloEvaloI x1;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           ___evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           ___evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           __evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           _evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           __evaloEvaloI x1;
                           ___evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           __evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           ___evaloEvaloI x1;
                           __evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Disj y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           _evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Conj y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           _evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           _evaloEvaloI x1;
                           ___evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           __evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           _evaloEvaloI x1;
                           __evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           _evaloEvaloI x1;
                           _evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           __evaloEvaloI x1;
                           __evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           ___evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           ___evaloEvaloI x1;
                           ___evaloEvaloI x2;
                           return ()},
                       do {(x1, x2) <- case x0 of
                                       {Impl y1 y2 -> return (y1, y2); _ -> mzero};
                           evaloEvaloI x1;
                           evaloEvaloI x2;
                           return ()}]
evaloEvaloO = msum [do {let {x3 = Trueo};
                        let {x4 = x3};
                        let {x0 = Lit x4};
                        return x0},
                    do {x1 <- _evaloEvaloO; let {x0 = Neg x1}; return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- ___evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- ___evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- __evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- _evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- __evaloEvaloO;
                        x2 <- ___evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- __evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- ___evaloEvaloO;
                        x2 <- __evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- _evaloEvaloO;
                        let {x0 = Disj x1 x2};
                        return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Conj x1 x2};
                        return x0},
                    do {x1 <- _evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- _evaloEvaloO;
                        x2 <- ___evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- __evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- _evaloEvaloO;
                        x2 <- __evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- _evaloEvaloO;
                        x2 <- _evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- __evaloEvaloO;
                        x2 <- __evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- ___evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- ___evaloEvaloO;
                        x2 <- ___evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0},
                    do {x1 <- evaloEvaloO;
                        x2 <- evaloEvaloO;
                        let {x0 = Impl x1 x2};
                        return x0}]