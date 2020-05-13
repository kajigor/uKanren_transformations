open GT

open OCanren
open OCanren.Std
open Tester

open Bottles

(******************************************)

let show_bottle = function
 | Fst -> "1"
 | Snd -> "2"

let show_stepType = function
 | Fill  -> "F"
 | Empty -> "E"
 | Pour  -> "P"

let show_step = function
 | (s, b) -> Printf.sprintf "%s%s" (show_bottle b) (show_stepType s)

let myshow x = Printf.printf "Aha" (* show List.ground show_step x *)

(******************************************)

(* let rec int2nat n = if n = 0 then o () else s @@ int2nat @@ n - 1 *)

(** For high order conversion **)
(* let checkAnswer q n r = checkAnswer ((===) q) ((===) n) r *)

(** For call-by-need conversion **)
(* let checkAnswer q n r = (snd checkAnswer) ([Obj.magic q], (===) q) ([], (===) n) r *)

let _ =
  run_exn myshow (1) q qh ("answers", fun q ->
  (* topLevel (Std.nat 7) q !!true *)
    topLevel (Std.List.nil ()) q (Std.nat 7)
  )
