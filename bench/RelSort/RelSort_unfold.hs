module RelSort_unfold where

import Term
import Control.Monad (msum, guard, mzero, MonadPlus)

sortoIOffline :: MonadPlus m => Term -> m ()
sortoIOffline x0 = msum [do {sorto0I x0;
                      (x1, x2, x3, x4) <- fn1OOOO;
                      return ()},
                  do {(x1, x2, x3, x4) <- sorto1IOOOO x0;
                      fn2IIII x1 x2 x3 x4;
                      return ()}]
                      
fn1OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn1OOOO = msum [do {(x4, x1, x2, x3) <- fn3OOOO;
                    x0 <- fn9OI x4;
                    return (x0, x1, x2, x3)},
                do {(x4, x1, x2, x3) <- fn10OOOO;
                    x0 <- fn13OI x4;
                    return (x0, x1, x2, x3)}]
                    
fn10OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn10OOOO = msum [do {(x0, x1, x2, x3) <- fn11OOOO;
                     return (x0, x1, x2, x3)},
                 do {(x0, x1, x2, x3) <- fn12OOOO; return (x0, x1, x2, x3)}]
                 
                 
fn11OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn11OOOO = msum [do {let {x1 = Zero};
                     let {x0 = x1};
                     (x2, x3) <- fn6OO;
                     return (x0, x1, x2, x3)}]
                     
fn12OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn12OOOO = msum [do {x1 <- fn122O;
                     let {x0 = Zero};
                     (x2, x3) <- fn9OO;
                     return (x0, x1, x2, x3)}]
                     
fn122O :: MonadPlus m => m Term
fn122O = msum [do {let {x33 = Zero};
                   let {x32 = Succ x33};
                   let {x1 = Succ x32};
                   return x1}]
                   
fn13OI :: MonadPlus m => Term -> m Term
fn13OI x1 = msum [do {fn132I x1; let {x0 = Zero}; return x0},
                  do {guard (x1 == Zero);
                      let {x35 = Zero};
                      let {x0 = Succ x35};
                      return x0}]

fn132I :: MonadPlus m => Term -> m ()
fn132I x1 = msum [do {x34 <- case x1 of
                             {Succ y34 -> return y34; _ -> mzero};
                      guard (x34 == Zero);
                      return ()}]


fn2IIII :: MonadPlus m => Term -> Term -> Term -> Term -> m ()
fn2IIII x0 x1 x2 x3 = msum [do {x4 <- fn14OIII x1 x2 x3;
                                fn6II x0 x4;
                                return ()}]

fn14OIII :: MonadPlus m => Term -> Term -> Term -> m Term 
fn14OIII x1 x2 x3 = msum [do {x0 <- fn15OIII x1 x2 x3; return x0},
                          do {x0 <- fn16OIII x1 x2 x3; return x0}]
                          
                          
fn15OIII :: MonadPlus m => Term -> Term -> Term -> m Term 
fn15OIII x1 x2 x3 = msum [do {guard (x1 == Zero);
                              let {x0 = x1};
                              fn13II x2 x3;
                              return x0}]


fn13II :: MonadPlus m => Term -> Term -> m () 
fn13II x0 x1 = msum [do {fn132I x1; guard (x0 == Zero); return ()},
                     do {guard (x1 == Zero);
                         x35 <- case x0 of
                                {Succ y35 -> return y35; _ -> mzero};
                         guard (x35 == Zero);
                         return ()}]
                         
fn16OIII :: MonadPlus m => Term -> Term -> Term -> m Term 
fn16OIII x1 x2 x3 = msum [do {fn162I x1;
                              let {x0 = Zero};
                              fn9II x2 x3;
                              return x0}]
                              
fn162I :: MonadPlus m => Term -> m () 
fn162I x1 = msum [do {x36 <- case x1 of
                             {Succ y36 -> return y36; _ -> mzero};
                      guard (x36 == Zero);
                      return ()}]

fn3OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn3OOOO = msum [do {(x0, x1, x2, x3) <- fn4OOOO;
                    return (x0, x1, x2, x3)},
                do {(x0, x1, x2, x3) <- fn7OOOO; return (x0, x1, x2, x3)}]
                
fn4OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn4OOOO = msum [do {let {x1 = Zero};
                    let {x0 = x1};
                    (x2, x3) <- fn5OO;
                    return (x0, x1, x2, x3)},
                do {x1 <- fn41O;
                    let {x0 = x1};
                    (x2, x3) <- fn5OO;
                    return (x0, x1, x2, x3)},
                do {x1 <- fn42O;
                    let {x0 = Zero};
                    (x2, x3) <- fn6OO;
                    return (x0, x1, x2, x3)}]
                    
                    
fn41O :: MonadPlus m => m Term 
fn41O = msum [do {let {x13 = Zero};
                  let {x1 = Succ x13};
                  return x1}]
                  
fn42O :: MonadPlus m => m Term 
fn42O = msum [do {let {x14 = Zero};
                  let {x1 = Succ x14};
                  return x1}]
                  
fn5OO :: MonadPlus m => m (Term, Term) 
fn5OO = msum [do {let {x16 = Zero};
                  let {x15 = Succ x16};
                  let {x1 = Succ x15};
                  let {x17 = Zero};
                  let {x0 = Succ x17};
                  return (x0, x1)},
              do {let {x18 = Zero};
                  let {x1 = Succ x18};
                  let {x20 = Zero};
                  let {x19 = Succ x20};
                  let {x0 = Succ x19};
                  return (x0, x1)}]
                  
fn6II :: MonadPlus m => Term -> Term -> m () 
fn6II x0 x1 = msum [do {fn62I x1; guard (x0 == Zero); return ()},
                    do {guard (x1 == Zero);
                        x23 <- case x0 of
                               {Succ y23 -> return y23; _ -> mzero};
                        x24 <- case x23 of
                               {Succ y24 -> return y24; _ -> mzero};
                        guard (x24 == Zero);
                        return ()}]
                        
fn6OO :: MonadPlus m => m (Term, Term)
fn6OO = msum [do {x1 <- fn62O; let {x0 = Zero}; return (x0, x1)},
              do {let {x1 = Zero};
                  let {x24 = Zero};
                  let {x23 = Succ x24};
                  let {x0 = Succ x23};
                  return (x0, x1)}]
                  
fn62I :: MonadPlus m => Term -> m ()
fn62I x1 = msum [do {x21 <- case x1 of
                            {Succ y21 -> return y21; _ -> mzero};
                     x22 <- case x21 of
                            {Succ y22 -> return y22; _ -> mzero};
                     guard (x22 == Zero);
                     return ()}]
                     
fn62O :: MonadPlus m => m Term 
fn62O = msum [do {let {x22 = Zero};
                  let {x21 = Succ x22};
                  let {x1 = Succ x21};
                  return x1}]
                  
fn7OOOO :: MonadPlus m => m (Term, Term, Term, Term)
fn7OOOO = msum [do {x1 <- fn72O;
                    let {x0 = Zero};
                    (x2, x3) <- fn13OO;
                    return (x0, x1, x2, x3)},
                do {let {x28 = Zero};
                    let {x27 = Succ x28};
                    let {x1 = Succ x27};
                    let {x29 = Zero};
                    let {x0 = Succ x29};
                    (x2, x3) <- fn8OO;
                    return (x0, x1, x2, x3)}]
                    
fn13OO :: MonadPlus m => m (Term, Term)
fn13OO = msum [do {x1 <- fn132O; let {x0 = Zero}; return (x0, x1)},
               do {let {x1 = Zero};
                   let {x35 = Zero};
                   let {x0 = Succ x35};
                   return (x0, x1)}]
fn132O :: MonadPlus m => m (Term)
fn132O = msum [do {let {x34 = Zero};
                   let {x1 = Succ x34};
                   return x1}]
fn72O :: MonadPlus m => m (Term)
fn72O = msum [do {let {x26 = Zero};
                  let {x25 = Succ x26};
                  let {x1 = Succ x25};
                  return x1}]
fn8OO :: MonadPlus m => m (Term, Term)
fn8OO = msum [do {let {x30 = Zero};
                  let {x1 = Succ x30};
                  let {x31 = Zero};
                  let {x0 = Succ x31};
                  return (x0, x1)}]
fn9II :: MonadPlus m => Term -> Term -> m ()
fn9II x0 x1 = msum [do {guard (x1 == Zero);
                        guard (x0 == Zero);
                        return ()}]
fn9OI :: MonadPlus m => Term -> m Term
fn9OI x1 = msum [do {guard (x1 == Zero);
                     let {x0 = Zero};
                     return x0}]
                     
fn9OO :: MonadPlus m => m (Term, Term)
fn9OO = msum [do {let {x1 = Zero};
                  let {x0 = Zero};
                  return (x0, x1)}]


sortoOOffline :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term  -> m Term
sortoOOffline gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4 gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4 = msum [do {x0 <- sorto0O gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4;
                                                                                                                                                           (x1,
                                                                                                                                                            x2,
                                                                                                                                                            x3,
                                                                                                                                                            x4) <- fn1OOOO;
                                                                                                                                                           return x0},
                                                                                                                                                       do {(x0,
                                                                                                                                                            x1,
                                                                                                                                                            x2,
                                                                                                                                                            x3,
                                                                                                                                                            x4) <- sorto1OOOOO gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4;
                                                                                                                                                           fn2IIII x1 x2 x3 x4;
                                                                                                                                                           return x0}]
sorto0I :: MonadPlus m => Term -> m ()
sorto0I x0 = msum [do {(x1, x5) <- case x0 of
                                   {Cons y1 y5 -> return (y1, y5); _ -> mzero};
                       (x2, x6) <- case x5 of
                                   {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                       (x3, x7) <- case x6 of
                                   {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                       (x4, x8) <- case x7 of
                                   {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                       guard (x8 == Nil);
                       return ()}]
sorto0O :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term
sorto0O gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4 = msum [do {let {x8 = Nil};
                                                                                (x7,
                                                                                 x4) <- do {x4 <- gen_sorto0O_x4;
                                                                                            let {x7 = Cons x4 x8};
                                                                                            return (x7,
                                                                                                    x4)};
                                                                                (x6,
                                                                                 x3) <- do {x3 <- gen_sorto0O_x3;
                                                                                            let {x6 = Cons x3 x7};
                                                                                            return (x6,
                                                                                                    x3)};
                                                                                (x5,
                                                                                 x2) <- do {x2 <- gen_sorto0O_x2;
                                                                                            let {x5 = Cons x2 x6};
                                                                                            return (x5,
                                                                                                    x2)};
                                                                                (x0,
                                                                                 x1) <- do {x1 <- gen_sorto0O_x1;
                                                                                            let {x0 = Cons x1 x5};
                                                                                            return (x0,
                                                                                                    x1)};
                                                                                return x0}]
sorto1IOOOO :: MonadPlus m => Term -> m (Term, Term, Term, Term)
sorto1IOOOO x0 = msum [do {(x1, x9) <- case x0 of
                                       {Cons y1 y9 -> return (y1, y9); _ -> mzero};
                           (x2, x10) <- case x9 of
                                        {Cons y2 y10 -> return (y2, y10); _ -> mzero};
                           (x3, x11) <- case x10 of
                                        {Cons y3 y11 -> return (y3, y11); _ -> mzero};
                           (x4, x12) <- case x11 of
                                        {Cons y4 y12 -> return (y4, y12); _ -> mzero};
                           guard (x12 == Nil);
                           return (x1, x2, x3, x4)}]
sorto1OOOOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term, Term)
sorto1OOOOO gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4 = msum [do {let {x12 = Nil};
                                                                                                    (x11,
                                                                                                     x4) <- do {x4 <- gen_sorto1OOOOO_x4;
                                                                                                                let {x11 = Cons x4 x12};
                                                                                                                return (x11,
                                                                                                                        x4)};
                                                                                                    (x10,
                                                                                                     x3) <- do {x3 <- gen_sorto1OOOOO_x3;
                                                                                                                let {x10 = Cons x3 x11};
                                                                                                                return (x10,
                                                                                                                        x3)};
                                                                                                    (x9,
                                                                                                     x2) <- do {x2 <- gen_sorto1OOOOO_x2;
                                                                                                                let {x9 = Cons x2 x10};
                                                                                                                return (x9,
                                                                                                                        x2)};
                                                                                                    (x0,
                                                                                                     x1) <- do {x1 <- gen_sorto1OOOOO_x1;
                                                                                                                let {x0 = Cons x1 x9};
                                                                                                                return (x0,
                                                                                                                        x1)};
                                                                                                    return (x0,
                                                                                                            x1,
                                                                                                            x2,
                                                                                                            x3,
                                                                                                            x4)}]