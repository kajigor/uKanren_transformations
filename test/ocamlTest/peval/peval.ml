open OCanren
open GT
(*
module Simple =
  struct
   
    @type ('a, 'b) fa =
    | Conj of 'a * 'a
    | Disj of 'a * 'a
    | Var  of 'b with show, gmap

    @type name = [ `x | `y | `z ] with show, gmap

    @type f = (f, string logic) fa logic with show, gmap
    
    @type f1 = (f1, name logic) fa logic with show, gmap

    module F = Fmap2 (struct type ('a, 'b) t = ('a, 'b) fa let fmap f g x = gmap(fa) f g x end)

    let rec reify_f f = F.reify reify_f reify f
    
    module Helper = struct 
        let run_state show_var r = 
          Printf.printf "-----------------------------\n"; 
          List.iter (fun s -> Printf.printf "%s\n" @@ show(Std.List.logic) (show(logic) show_var) @@ (s#reify (Std.List.reify reify))) @@ RStream.take ~n:10 @@ r (fun q r s -> s)

        let run_fm_name r = 
          Printf.printf "-----------------------------\n"; 
          List.iter (fun s -> Printf.printf "%s\n" @@ show(f1) (s#reify reify_f)) @@ RStream.take ~n:10 @@
                     r (fun _ _ s -> s)

        let run_fm r = 
          Printf.printf "-----------------------------\n"; 
          List.iter (fun s -> Printf.printf "%s\n" @@ show(f) (s#reify reify_f)) @@ RStream.take ~n:10 @@
                    r (fun _ _ s -> s)
      end 
                  
    let conj x y = inj @@ F.distrib (Conj (x, y))
    let disj x y = inj @@ F.distrib (Disj (x, y))
    let var  x   = inj @@ F.distrib (Var x)

    let rec evalo st fm = 
      fresh (x y v) (
        conde [
          fm === conj x y &&&  evalo st x &&& evalo st y;
          fm === disj x y &&& (evalo st x ||| evalo st y);
          fm === var  v   &&& Std.List.membero st v
      ]) 

    let _ =
      Helper.run_state (show(name))@@ 
      run qrs
        (fun q r s -> evalo s (disj (var !!`x) (var !!`y)));
        
      Helper.run_state (show(string)) @@ 
      run qrs
        (fun q r s -> 
          (Std.List.lengtho s (Std.nat 2)) &&& 
          (r =/= q) &&& 
          evalo s (conj (var r) (var q)));
  
      Helper.run_fm @@ 
      run qrs
        (fun q r s -> 
          (r =/= q) &&& 
          evalo (Std.(%<) r q) s)
  end

module Elaborated =
  struct
   
    @type ('a, 'b) fa =
    | Conj of 'a * 'a
    | Disj of 'a * 'a
    | Neg  of 'a
    | Var  of 'b 
    with show, gmap

    @type name = [ `x | `y | `z ] with show, gmap

    @type f = (f, name logic) fa logic with show, gmap

    module F = Fmap2 (struct type ('a, 'b) t = ('a, 'b) fa let fmap f g x = gmap(fa) f g x end)

    let rec reify_f f = F.reify reify_f reify f
                  
    let conj x y = inj @@ F.distrib (Conj (x, y))
    let disj x y = inj @@ F.distrib (Disj (x, y))
    let var  x   = inj @@ F.distrib (Var x)
    let neg  x   = inj @@ F.distrib (Neg x)


    let rec eval st = function
    | Conj (l, r) -> eval st l && eval st r
    | Disj (l, r) -> eval st l || eval st r
    | Neg   e     -> not (eval st e)
    | Var   x     -> List.assoc x st
                       
    let empty = []
    
    let extend v n b = (n, b) :: v
     
    open List
     
    let rec solve env b = function
    | Var n -> ( match assoc_opt n env with 
                 | None -> [extend env n b]
                 | Some b' when b == b' -> [env] 
                 | _   -> [])
    | Conj (l, r) when b ->
        concat @@ 
        map (fun env -> solve env b r) @@ 
        solve env b l
    | Conj (l, r) -> solve env b l @ solve env b r
    | Neg e -> solve env (not b) e
    | Disj (l, r) -> solve env b (Neg (Conj (Neg l, Neg r))) 
                   
    let check f = List.map (fun env -> eval env f ) (solve empty true f)               
    
    let s f = solve empty true f 
    
    let f1 = Disj (Neg (Var `x), (Var `x))
    let f2 = Conj (Neg (Var `x), (Var `x))
    (*
    let show_env c = show(list) (show(pair) (show(name)) string_of_bool) c
      *)           
    let check f = List.for_all (fun env -> eval env f) (s f)

    let rec evalo st fm u = 
      fresh (x y z v w) (
        conde [
          fm === conj x y &&& evalo st x v 
             &&& evalo st y w &&& Std.Bool.ando v w u;
          fm === disj x y &&& evalo st x v 
             &&& evalo st y w &&& Std.Bool.oro  v w u;
          fm === neg  x   &&& evalo st x v 
             &&& Std.Bool.noto v u ; 
          fm === var  z   &&& Std.List.assoco z st u
      ])

    let run_fm textRepr r = 
          Printf.printf "\n-----------------------------\n%s\n" textRepr;
          List.iter (fun s ->
              Printf.printf "%s\n" @@   (show(logic) (show(bool)))
                                        (s#reify reify)) @@ RStream.take ~n:10 @@
          r (fun st r -> r) 
          
    let _ =
          run_fm "not x && x" @@
          run qr (fun st r -> 
            evalo st (conj (neg @@ var !!`x) (var !!`x)) r) ;                 
          
          run_fm "not x && y" @@
          run qr (fun st r -> 
            evalo st (conj (neg @@ var !!`x) (var !!`y)) r);
           
          run_fm "not x || x" @@
          run qr (fun st r -> 
            evalo st (disj (neg @@ var !!`x) (var !!`x)) r);
  end *) 
 
module Transformed = 
  struct 

    @type ('a, 'b) fa =
    | Conj of 'a * 'a
    | Disj of 'a * 'a
    | Neg  of 'a
    | Var  of 'b 
    with show, gmap

    @type f = (f, Std.Bool.logic) fa logic with show, gmap

    module F = Fmap2 (struct type ('a, 'b) t = ('a, 'b) fa let fmap f g x = gmap(fa) f g x end)

    let conj x y = inj @@ F.distrib (Conj (x, y))
    let disj x y = inj @@ F.distrib (Disj (x, y))
    let var  x   = inj @@ F.distrib (Var x)
    let neg  x   = inj @@ F.distrib (Neg x)


    let rec reify_f f = F.reify reify_f reify f


    let topLevel x0 x1 =
      let rec _evalo y2 y3 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y3 === conj q1 q2 &&& ___evaloEvalo y2 q1 q2
          ||| (y3 === disj q1 q2 &&& (__evalo y2 q1 &&& _evalo y2 q2 ||| __evaloEvalo y2 q1 q2 ||| ___evaloEvalo y2 q1 q2))
          ||| (y3 === neg q1 &&& __evalo y2 q1)
          ||| (y3 === var q3 &&& (q3 === Std.Nat.zero &&& (y2 === Std.( % ) !!true q4) ||| (q3 === Std.Nat.succ q5 &&& (y2 === Std.( % ) q6 q4) &&& _elemo q4 q5)))
          )
      and __evalo y4 y5 =
        fresh (q1 q2 q3 q4 q5 q6)
          ( y5 === conj q1 q2
          &&& (_evaloEvalo y4 q1 q2 ||| evaloEvalo y4 q1 q2 ||| (_evalo y4 q1 &&& __evalo y4 q2))
          ||| (y5 === disj q1 q2 &&& _evaloEvalo y4 q1 q2)
          ||| (y5 === neg q1 &&& _evalo y4 q1)
          ||| (y5 === var q3 &&& (q3 === Std.Nat.zero &&& (y4 === Std.( % ) !!false q4) ||| (q3 === Std.Nat.succ q5 &&& (y4 === Std.( % ) q6 q4) &&& elemo q4 q5)))
          )
      and evaloEvalo y6 y7 y8 = __evalo y6 y7 &&& _evalo y6 y8
      and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& __evalo y9 y11
      and elemo y12 y13 =
        fresh (q1 q2 q3) (y13 === Std.Nat.zero &&& (y12 === Std.( % ) !!false q1) ||| (y13 === Std.Nat.succ q2 &&& (y12 === Std.( % ) q3 q1) &&& elemo q1 q2))
      and __evaloEvalo y14 y15 y16 = _evalo y14 y15 &&& __evalo y14 y16
      and ___evaloEvalo y17 y18 y19 = _evalo y17 y18 &&& _evalo y17 y19
      and _elemo y20 y21 =
        fresh (q1 q2 q3) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1) ||| (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1) &&& _elemo q1 q2))
      in
      _evalo x0 x1

    let show_env c = show(Std.List.logic) (show(logic) (show(bool))) c

    let run_fm textRepr r = 
          Printf.printf "\n-----------------------------\n%s\n" textRepr;
          List.iter (fun s ->
              Printf.printf "%s\n" @@   show_env
                                        (s#reify (Std.List.reify reify))) @@ RStream.take ~n:10 @@
          r (fun st -> st) 
         
    let run_fm1 textRepr r = 
          Printf.printf "\n-----------------------------\n%s\n" textRepr;
          List.iter (fun s ->
              Printf.printf "%s\n" @@   show_env
                                        (s#reify (Std.List.reify reify))) @@ RStream.take ~n:3 @@
          r (fun st r -> st)
         
    let run_fm2 textRepr r = 
          Printf.printf "\n-----------------------------\n%s\n" textRepr;
          List.iter (fun s ->
              Printf.printf "%s\n" @@   show_env
                                        (s#reify (Std.List.reify reify))) @@ RStream.take ~n:3 @@
          r (fun st r t -> st)


    let _ =
          let x = Std.nat 0 in 
          let y = Std.nat 1 in

          run_fm "not x && x" @@
          run q (fun st -> 
            topLevel st (conj (neg @@ var x) (var x))) ;                

          run_fm "not x && y" @@
          run q (fun st -> 
            topLevel st (conj (neg @@ var x) (var y)));
           
          run_fm "not x || x" @@
          run q (fun st -> 
            topLevel st (disj (neg @@ var x) (var x)));


        (*
          (* won't terminate *)
          run_fm1 "\\forall x . not x && x" @@
          run qr (fun st r -> 
            topLevel st (conj (neg @@ var r) (var r))) ;
        *)

          run_fm2 "\\forall x y . not x && y" @@
          run qrs (fun st r t -> 
            topLevel st (conj (neg @@ var r) (var t))) ;
 
          run_fm1 "\\forall x . not x || x" @@
          run qr (fun st r -> 
            topLevel st (disj (neg @@ var r) (var r))) ;

  end                                        
