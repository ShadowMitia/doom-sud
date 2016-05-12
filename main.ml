open Options
open Point
open Player
open Segment
open Bsp
open Graphics
open Parse_lab
(*

let () =
  let point = Point.new_point 10 5 in
  let player = Player.new_player point 0 in

  print_int player.pa;print_newline ();Player.rotate Left player;print_int player.pa;print_newline ();

  let seg = Segment.new_segment 10 10 100 100 in
  match (Segment.get_position point seg) with
  | L -> print_string "L\n"
  | C -> print_string "C\n"
  | R -> print_string "R\n"

(*
        let test () =
          let t = ref 1 in
          fun () -> t := !t + 1; !t

          print_int (test ())
            print_int (test ())
 *)

let seg1 = Segment.new_segment ~id:"AB" 0 0 50 50
let seg2 = Segment.new_segment ~id:"CD" 0 50 50 0
let seg3 = Segment.new_segment ~id:"EF" 0 0 100 0
let seg4 = Segment.new_segment ~id:"GH" 0 50 100 50
let seg5 = Segment.new_segment ~id:"IJ" 50 50 100 100
let seg6 = Segment.new_segment ~id:"KL" 0 100 200 100
let seg7 = Segment.new_segment ~id:"MN" 50 125 50 200
let seg8 = Segment.new_segment ~id:"OP" 25 50 75 100
let res1, res2 = Segment.split_segment seg1 seg2
let res3, res4 = Segment.split_segment seg3 seg4
let res5, res6 = Segment.split_segment seg5 seg6

let test_intersection segm1 segm2 =
  match segm1, segm2 with
  | None, None -> print_string "Pas d'intersection\n"
  | Some a, Some b -> print_string "Coupé";print_segment a; print_string "coupé2"; print_segment b
  | Some a, None -> print_string "G";print_segment a
  | None, Some a -> print_string "D"; print_segment a

let () =
  test_intersection res1 res2

let () =
  test_intersection res3 res4

let () =
  test_intersection res5 res6

let seg_list = [seg1;seg2;seg3;seg4;seg5;seg6;seg7;seg8]

let () =
    Printf.printf "List\n";
    let l,r = Segment.split seg1 seg_list in
    print_string ("Gauche de " ^ seg1.id^"\n"); print_list l;print_string ("Droite de "^seg1.id^"\n");print_list r

 *)

let print_bsp_to_file bsp oc =
  Printf.fprintf oc "%s\n" "#+begin_src dot :file tree.svg :cmdline -Tsvg :exports none :results silent";
  Printf.fprintf oc "%s\n"  "digraph tree {";
  (*Printf.fprintf oc "%s\n"  "rankdir=\"LR\";";*)
  Printf.fprintf oc "%s\n"  "size=\"6,6\";";
  Printf.fprintf oc "%s\n"  "node [color=lightblue2, style=filled];";
  let rec print_bsp_rec = function
    | E -> ()
    | N(root, left, right) ->
       let lroot = (fun x -> match x with E -> root.id | N(r, _, _) -> r.id) left in
       let rroot = (fun x -> match x with E -> root.id | N(r, _, _) -> r.id) right in
       Printf.fprintf oc "%s\n"  ("\"" ^ root.id ^ "\"->\"" ^ lroot ^ "\"[label=\"left\"];");
       Printf.fprintf oc "%s\n"  ("\"" ^ root.id ^ "\"->\"" ^ rroot ^ "\"[label=\"right\"];");
       Printf.fprintf oc "%s\n"  ("{rank=same;\"" ^ lroot ^ "\";\"" ^ rroot ^"\"}");
       print_bsp_rec left;
       print_bsp_rec right;
  in print_bsp_rec bsp;
     Printf.fprintf oc "%s\n"  "}\n";
     Printf.fprintf oc "%s\n" "#+end_src";
     Printf.fprintf oc "%s\n" "[[file:tree.svg]]"

(*
let () =
  let bsp = Bsp.build_bsp seg_list in
  print_bsp bsp
 *)

(* BEGINNING OF PROGRAM *)

let rec print_list l =
  match l with
  | [] -> ()
  | el :: s -> print_segment el; print_list s

let generate_segments l =
  let rec generate_segments_rec l acc =
    match l with
    | [] -> acc
    | (x1,y1,x2,y2) :: r -> generate_segments_rec r (Segment.new_segment x1 y1 x2 y2 :: acc)
  in generate_segments_rec l []

let () =
  (** INITIALISATION **)
  let (player_x, player_y, player_angle), labyrinth = Parse_lab.read_lab Options.cin in
  let player = Player.new_player (Point.new_point player_x player_y) player_angle in
  let gen_segs = generate_segments labyrinth in
  let bsp = Bsp.build_bsp gen_segs in
  print_list (gen_segs);
  (* Write message to file *)
  let oc = open_out "debug.org" in    (* create or truncate file, return channel *)
  print_bsp_to_file bsp oc ;  (* write something *)
  close_out oc;                (* flush and close the channel *)
  print_string "START OF MAIN PRORAM -----\n";
  (** MAIN LOOP **)
  Graphics.open_graph (Printf.sprintf " %dx%d" Options.win_w Options.win_h);
  Graphics.set_window_title "Doom-Sud";
  Graphics.auto_synchronize false;
  Render.display bsp player; Graphics.synchronize();
  try
    while true do
      ( (** EVENTS **)
        let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
        match ev with
        | keypressed -> match ev.key with
                        | _ -> print_char ev.key; print_newline ());
      (** UPDATE **)
      print_string "list\n";
      print_list (gen_segs);
      print_string "end_list\n";
      (** RENDER **)
      Render.display bsp player;
      (** DRAW **)
      Graphics.synchronize();
    done
  ;
  with Exit -> Graphics.close_graph(); exit 0
