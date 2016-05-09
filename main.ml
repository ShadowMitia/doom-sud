open Options
open Point
open Player
open Segment
       open Bsp


  let point = Point.new_point 10 5
let player = Player.new_player point 0

let () =
  print_int player.pa;print_newline ();Player.rotate Left player;print_int player.pa;print_newline ()

let seg = Segment.new_segment 10 10 100 100
let () =
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
let print_segment seg =
  Printf.printf "[id=%s -> xA=%d yA=%d xB=%d yB=%d]\n" seg.id seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y

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

let rec print_list l =
  match l with
  | [] -> ()
  |el :: s -> print_segment el; print_list s

let () =
    Printf.printf "List\n";
    let l,r = Segment.split seg1 seg_list in
    print_string ("Gauche de " ^ seg1.id^"\n"); print_list l;print_string ("Droite de "^seg1.id^"\n");print_list r

let print_bsp bsp =
  let rec print_bsp_rec bsp depth =
  match bsp with
  | E -> ()
  | N(root, left, right) -> print_bsp_rec left (depth ^ "\t"); print_string depth; print_segment root; print_bsp_rec right (depth ^ "\t"); print_newline ()
in print_bsp_rec bsp "\t"

let () =
  let bsp = Bsp.build_bsp seg_list in
  print_bsp bsp
