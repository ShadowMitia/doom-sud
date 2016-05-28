open Point
open Segment
open Bsp
open Player
open Graphics

(* Rename to rotatio around point? *)
let rotation_around_player seg p =
  let xp = float_of_int (p.pos.x) in
  let yp = float_of_int (p.pos.y) in
  let a  = -p.pa in
  let xo, yo, xd, yd = Segment.get_real_coord seg in
  (* rotation around the player placed at the origin *)
  (* translation + rotation *)
  (* DON'T DIRECTLY CHANGE XO, YO, XD, YD OR BAD THINGS WILL HAPPEN *)
  let xo' = (xo -. xp) *. (Trigo.dcos a) -. (yo -. yp) *. (Trigo.dsin a) in
  let yo' = (yo -. yp) *. (Trigo.dcos a) +. (xo -. xp) *. (Trigo.dsin a) in
  let xd' = (xd -. xp) *. (Trigo.dcos a) -. (yd -. yp) *. (Trigo.dsin a) in
  let yd' = (yd -. yp) *. (Trigo.dcos a) +. (xd -. xp) *. (Trigo.dsin a) in
  let xo = int_of_float (xo' +. xp) in
  let yo = int_of_float (yo' +. yp) in
  let xd = int_of_float (xd' +. xp) in
  let yd = int_of_float (yd' +. yp) in
  Segment.new_segment xo yo xd yd

(* Display a minimap in the bottom left *)
let display_minimap bsp p =
  Graphics.set_color Graphics.black;
  Graphics.draw_rect 0 0 200 200;
  Graphics.set_color Graphics.red;
  Bsp.iter (fun seg ->
      let scale_down = Options.scale in
      let xo = seg.porig.x / scale_down in
      let yo = seg.porig.y / scale_down in
      let xd = seg.pdest.x / scale_down in
      let yd = seg.pdest.y / scale_down in
      Graphics.draw_segments [|xo, yo, xd, yd|]) bsp;
  Graphics.set_color Graphics.black;
  Graphics.fill_circle (p.pos.x / Options.scale) (p.pos.y / Options.scale) 3

(* Display player in the 2D space, in the middle of the screen, with an arrow showing a direction *)
let display_player player =
  let p = Player.new_player (Point.new_point (Options.win_w / 2) (Options.win_h / 2)) player.pa in
  (* Draw point that represents player *)
  Graphics.set_color Graphics.yellow;
  Graphics.fill_circle (*player.pos.x player.pos.y*) p.pos.x p.pos.y 10;
  (* Draw line to represent direction *)
  Graphics.set_color Graphics.cyan;
  let arrow = Segment.new_segment p.pos.x p.pos.y (p.pos.y + 50) p.pos.y in
  (* let arrow = rotation_around_player arrow p in *)
  (* Graphics.draw_segments [| arrow.porig.x + (Options.win_w / 2), arrow.porig.y + (Options.win_h / 2), arrow.pdest.x + (Options.win_w / 2), arrow.pdest.y + (Options.win_h / 2)|] *)
  let xo = arrow.porig.x in
  let yo = arrow.porig.y in
  let xd = arrow.pdest.x in
  let yd = arrow.pdest.y in
  Graphics.draw_segments [| xo, yo, xd, yd |]

(*
let draw_walls_simple bsp =
  Bsp.iter (fun seg ->
      Graphics.draw_segments [|seg.porig.x, seg.porig.y, seg.pdest.x, seg.pdest.y |]) bsp;
  if Options.debug then begin
      Bsp.iter (fun seg ->
          Graphics.moveto (seg.porig.x + ((seg.pdest.x - seg.porig.x) / 2))
                          (seg.porig.y + ((seg.pdest.y - seg.porig.y) / 2));
          Graphics.draw_string seg.id) bsp
    end
 *)

let clipping2D seg =

  let xo, yo, xd, yd = Segment.get_real_coord seg in
  (*
  let xo = float_of_int (seg.porig.x) in
  let yo = float_of_int (seg.porig.y) in
  let xd = float_of_int (seg.pdest.x) in
  let yd = float_of_int (seg.pdest.y) in
   *)
  let angle = (Trigo.rtan ((yd -. yo) /. (xd -. xo))) in
  if xo < 1. && xd < 1. then
    None
  else if xo < 1. && xd >= 1. then
    (* To simplify *)
    if xd > Options.xmax then
      Some (Options.xmax, yo +. (1. -. xo) *. angle, xd, yd)
    else
      Some (1., yo +. (1. -. xo) *. angle, xd, yd)
  else if xd < 1. && xo >= 1. then
    (* To simplify *)
    if xo > Options.xmax then
     Some (xo, yo, Options.xmax, yd +. (1. -. xd) *. angle)
    else
      Some (xo, yo, 1., yd +. (1. -. xd) *. angle)
  else
    Some (xo, yo, xd, yd)

let horizontal_projection seg =
  let is = (float_of_int Options.win_w) /. 2. in
  let focal_dist = (is /. Trigo.dtan(Options.fov / 2)) in
  let xo = focal_dist in
  let yo = float_of_int seg.porig.y in
  let xd = focal_dist in
  let yd = float_of_int seg.pdest.y in
  let yo = is -. ((yo *. focal_dist) /. xo) in
  let yd = is -. ((yd *. focal_dist) /. xd) in
  let xo = int_of_float xo in
  let yo = int_of_float yo in
  let xd = int_of_float xd in
  let yd = int_of_float yd in
  Segment.new_segment xo yo xd yd
  (* { seg with porig = Point.new_point (int_of_float xo) (int_of_float yo); pdest=Point.new_point (int_of_float xd) (int_of_float yd) } *)

let display_walls bsp p =
  Bsp.iter (fun seg ->
      (* let xo, yo, xd, yd = Segment.get_real_coord seg in *)
      let seg = rotation_around_player seg p in
      (* let seg = horizontal_projection seg in *)
      let s = clipping2D seg in
      match s with
      | None -> ()
      | Some (xo, yo, xd, yd) ->
         let xo = int_of_float (xo) in
         let yo = int_of_float (yo) in
         let xd = int_of_float (xd) in
         let yd = int_of_float (yd) in
         let xo = xo - p.pos.x + (Options.win_w / 2) in
         let yo = yo - p.pos.y + (Options.win_h / 2) in
         let xd = xd - p.pos.x + (Options.win_w / 2) in
         let yd = yd - p.pos.y + (Options.win_h / 2) in
         (* Segment.print_segment seg; *)
         Graphics.draw_segments [| xo, yo, xd, yd|]) bsp

let generate_3d_wall s =
  let is = float_of_int Options.win_h in
  let focal_dist = ((is /. 2.) /. Trigo.dtan(Options.fov / 2)) in
  let ph = is /. 2. in
  let fh = float_of_int Options.ceiling_h in
  let ch = float_of_int Options.floor_h in
  let x = float_of_int s.porig.x in
  let zuo = ref ((is /. 2.) +. ((ch -. ph) *. focal_dist /. x)) in
  let zlo = ref ((is /. 2.) +. ((fh -. ph) *. focal_dist /. x)) in
  let x = float_of_int s.pdest.x in
  let zud = ref ((is /. 2.) +. ((ch -. ph) *. focal_dist /. x)) in
  let zld = ref ((is /. 2.) +. ((fh -. ph) *. focal_dist /. x)) in

  let co = ref (float_of_int s.porig.y) in
  let cd = ref (float_of_int s.pdest.y) in

  let du = (!zud -. !zuo) /. (!cd -. !co) in
  let di = (!zld -. !zlo) /. (!cd -. !co) in

  if (!co < 0.) then
    begin
      co := 0.;
      zuo := !zuo -. (!co *. du);
      zlo := !zlo -. (!co *. di)
    end
  else
    begin
      co := is;
      zuo := !zuo -. ((!co -. is) *. du);
      zlo := !zlo -. ((!co -. is) *. di);
    end;

  if (!cd < 0.) then
    begin
      cd := 0.;
      zud := !zud -. (!cd *. du);
      zld := !zld -. (!cd *. di)
    end
  else
    begin
      cd := is;
      zld := !zld -. (!cd -. is) *. du;
      zld := !zld -. ((!cd -. is) *. di)
    end;
  let co = int_of_float !co in
  let cd = int_of_float !cd in
  let zuo = (int_of_float !zuo) in
  let zlo = (int_of_float !zlo) in
  let zud = (int_of_float !zud) in
  let zld = (int_of_float !zld) in
  Graphics.set_color Graphics.red;
  if s.ci > 0. && s.ce = 1. then
    Graphics.draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud);
                     (cd, zld, cd, zud)|])
  else if s.ce < 1. && s.ci = 0. then
    Graphics.draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud);
                     (co, zlo, co, zuo)|])
  else if s.ci > 0. && s.ce < 1. then
    Graphics.draw_segments ([|(co, zlo, cd, zld);
                     (co, zuo, cd, zud)|])
  else
    Graphics.draw_poly ([|(co, zlo);
                 (co, zuo);
                 (cd, zud);
                 (cd, zld)|])
              (*
  Graphics.fill_poly [| (int_of_float !co, int_of_float !zlo);
                        (int_of_float !co, int_of_float !zuo);
                        (int_of_float !cd, int_of_float !zud);
                        (int_of_float !cd, int_of_float !z
               *)

let display_walls_3d bsp p =
  Bsp.iter (fun seg ->
      let seg = horizontal_projection seg in
      let s = clipping2D seg in
      match s with
      | None -> ()
      | Some (xo, yo, xd, yd) ->
         let seg = rotation_around_player (Segment.new_segment (int_of_float xo) (int_of_float yo) (int_of_float xd) (int_of_float yd)) p in
         let xo = seg.porig.x in
         let yo = seg.porig.y in
         let xd = seg.pdest.x in
         let yd = seg.pdest.y in
         generate_3d_wall {seg with porig=(Point.new_point xo yo); pdest=(Point.new_point xd yd)}) bsp


let display bsp p =
  (* BACKGROUND *)
  Graphics.set_color Graphics.green;
  Graphics.fill_rect 0 0 Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 0 (Options.win_h / 2) Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.black;
  (* WALLS *)
  display_walls bsp p;
  (* display_walls_3d bsp p; *)

  (* PLAYER *)
  display_player p;
  (* MINIMAP *)
  display_minimap bsp p
