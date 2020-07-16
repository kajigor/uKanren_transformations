open OCanren
open GT
open Helper
open OCanren.Std

(* let _ =
  run_graph 1 "original" @@
  run q (fun q -> Original.isPath (ocanren ([1;2;3])) q !!true) *)

let graph2 = ocanren ([       (0,1); (0,2); (0,3); (0,4); (0,5);
                       (1,0);        (1,2); (1,3); (1,4); (1,5);
                       (2,0); (2,1);        (2,3); (2,4); (2,5);
                       (3,0); (3,1); (3,2);        (3,4); (3,5);
                       (4,0); (4,1); (4,2); (4,3);        (4,5);
                       (5,0); (5,1); (5,2); (5,3); (5,4)
                     ])

let graph1 = (ocanren ([(0,1);(1,2);(0,2)]))

let graph = ocanren ([(0, 1);  (1, 2);  (2, 3);  (3, 4);  (4, 5);  (5, 6);  (6, 7);  (7, 8);  (8, 9);  (9, 10);  (10, 11);  (11, 12);  (12, 13);  (13, 14);  (14, 15);  (15, 16);  (16, 17);  (17, 18);  (18, 19);  (19, 0);  (0, 3);  (5, 16);  (10, 8);  (15, 19);  (3, 16);  (5, 6);  (1, 11);  (7, 18);  (18, 2);  (4, 7)])

let l = ocanren (7)

 let _ =
  run_path 5 "original: path" @@
  run q (fun q -> fresh (a1 a2 a3 a4 a5) (q === (ocanren ([a1;a2;a3;a4;a5])) &&& Original.topLevel q graph))

let _ =
  run_path 5 "conspd: path" @@
  run q (fun q -> fresh (a1 a2 a3 a4 a5) (q === (ocanren ([a1;a2;a3;a4;a5])) &&& Conspd.topLevel q graph))

let _ =
  run_path 5 "cpd: path" @@
  run q (fun q -> fresh (a1 a2 a3 a4 a5) ((q === (ocanren ([a1;a2;a3;a4;a5]))) &&& (Cpd.topLevel q graph)))

let inputs = [ "     cpd", Cpd.topLevel
             ; "original", Original.topLevel
             ; "  conspd", Conspd.topLevel
             ]

let _ =
  do_tables 3 (fun rel -> run q (fun q -> fresh (a1 a2 a3 a4 a5 a6 a7) (q === (ocanren ([a1;a2;a3;a4;a5;a6;a7])) &&& rel q graph))) inputs