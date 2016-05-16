open Point
open Segment
open Bsp
open Player
open Graphics

let display_minimap bsp p =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 200 200;
  Graphics.set_color Graphics.red;

  Bsp.iter (fun seg ->
      let scale_down = Options.scale in
      let xo = seg.porig.x / scale_down in
      let yo = seg.porig.y / scale_down in
      let xd = seg.pdest.x / scale_down in
      let yd = seg.pdest.y / scale_down in
      Graphics.draw_segments [|xo, yo, xd, yd|]) bsp;
  Graphics.set_color Graphics.green;
  Graphics.fill_circle (p.pos.x / Options.scale) (p.pos.y / Options.scale) 3

let display_player player =
  Graphics.set_color Graphics.yellow;
  Graphics.fill_circle player.pos.x player.pos.y 10;
  Graphics.set_color Graphics.cyan;
  let xo = player.pos.x in
  let yo = player.pos.y in
  let angle = -player.pa in
  let x' = float_of_int player.pos.x in
  let y' = float_of_int (player.pos.y + 50) in
  let xd = (x' -. (float_of_int xo)) *. Trigo.dcos(angle) -. (y' -. (float_of_int yo)) *. Trigo.dsin(angle) in
  let yd = (x' -. (float_of_int xo))  *. Trigo.dsin(angle) -.(y' -. (float_of_int yo)) *. Trigo.dcos(angle) in
  let xd = int_of_float (xd +. 0.5) in
  let yd = int_of_float (yd +. 0.5) in
  Graphics.draw_poly_line [|(xo, yo);
                            (xd + xo, yd + yo)
                          |]

let draw_walls_simple bsp =
  Bsp.iter (fun seg ->
      Graphics.draw_segments [|seg.porig.x, seg.porig.y, seg.pdest.x, seg.pdest.y |]) bsp;
  if Options.debug then begin
      Bsp.iter (fun seg ->
          Graphics.moveto (seg.porig.x + ((seg.pdest.x - seg.porig.x) / 2))
                          (seg.porig.y + ((seg.pdest.y - seg.porig.y) / 2));
          Graphics.draw_string seg.id) bsp
    end

let clipping2D seg =
  let xo, yo, xd, yd = Segment.get_real_coord seg in
  let angle = Trigo.rtan ((yd -. yo) /. (xd -. xo)) in
  if xo < 1. && xd < 1. then
    None
  else if xo < 1. && xd >= 1. then

    Some (1., yo +. (1. -. xo) *. angle, xd, yd)
  else if xd < 1. && xo >= 1. then
    Some (xo, yo, 1., yd +. (1. -. xd) *. angle)
  else
    Some (xo, yo, xd, yd)

let horizontal_projection seg =
  let w_2 = (float_of_int Options.win_w) /. 2. in
  let focal_dist = (w_2 /. Trigo.dtan(Options.fov / 2)) in
  let xo = focal_dist in
  let yo = float_of_int seg.porig.y in
  let yd = float_of_int seg.pdest.y in
  let xd = focal_dist in
  let yo = w_2 -. (yo *. focal_dist /. xo) in
  let yd = w_2 -. (yd *. focal_dist /. yd) in
  { seg with porig = Point.new_point (int_of_float xo) (int_of_float yo); pdest=Point.new_point (int_of_float xd) (int_of_float yd) }

let draw_walls bsp p =
  let xp = float_of_int p.pos.x in
  let yp = float_of_int p.pos.y in
  let a = -(p.pa) in
  Bsp.iter (fun seg ->
      (* let xo, yo, xd, yd = Segment.get_real_coord seg in *)
      (*
      let xo = float_of_int seg.porig.x in
      let yo = float_of_int seg.porig.y in
      let xd = float_of_int seg.pdest.x in
      let yd = float_of_int seg.pdest.y in
       *)
      let seg = horizontal_projection seg in
      let s = clipping2D seg in
      match s with
      | None -> ()
      | Some (xo, yo, xd, yd) ->
         let xo = (xo -. xp) *. Trigo.dcos(a) -. (yo -. yp) *. Trigo.dsin(a) in
         let yo = (xo -. xp) *. Trigo.dsin(a) +. (yo -. yp) *. Trigo.dcos(a) in
         let xd = (xd -. xp) *. Trigo.dcos(a) -. (yd -. yp) *. Trigo.dsin(a) in
         let yd = (xd -. xp) *. Trigo.dsin(a) +. (yd -. yp) *. Trigo.dcos(a) in
         let xo = int_of_float (xo +. xp) in
         let yo = int_of_float (yo +. yp) in
         let xd = int_of_float (xd +. xp +. 0.5) in
         let yd = int_of_float (yd +. yp +. 0.5) in
         (* Segment.print_segment seg; *)
         Graphics.draw_segments [| xo, yo, xd, yd|]) bsp

(* let draw_walls_with_clipping2d bsp p = *)

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
  Graphics.set_color Graphics.red;
  Graphics.fill_poly [| (int_of_float !co, int_of_float !zlo);
                        (int_of_float !co, int_of_float !zuo);
                        (int_of_float !cd, int_of_float !zud);
                        (int_of_float !cd, int_of_float !zld)
                     |]

let display_walls_3d bsp p =
  let xp = float_of_int p.pos.x in
  let yp = float_of_int p.pos.y in
  let a = -(p.pa) in
  Bsp.iter (fun seg ->
      let seg = horizontal_projection seg in
      let s = clipping2D seg in
      match s with
      | None -> ()
      | Some (xo, yo, xd, yd) ->
         let xo = (xo -. xp) *. Trigo.dcos(a) -. (yo -. yp) *. Trigo.dsin(a) in
         let yo = (xo -. xp) *. Trigo.dsin(a) +. (yo -. yp) *. Trigo.dcos(a) in
         let xd = (xd -. xp) *. Trigo.dcos(a) -. (yd -. yp) *. Trigo.dsin(a) in
         let yd = (xd -. xp) *. Trigo.dsin(a) +. (yd -. yp) *. Trigo.dcos(a) in
         let xo = int_of_float (xo +. xp) in
         let yo = int_of_float (yo +. yp) in
         let xd = int_of_float (xd +. xp +. 0.5) in
         let yd = int_of_float (yd +. yp +. 0.5) in
         generate_3d_wall {seg with porig=(Point.new_point xo yo); pdest=(Point.new_point xd yd)}) bsp


let display bsp p =
  (* BACKGROUND *)
  Graphics.set_color Graphics.green;
  Graphics.fill_rect 0 0 Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 0 (Options.win_h / 2) Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.black;
  (* WALLS *)
  display_walls_3d bsp p;

  (* PLAYER *)
  display_player p;
  (* MINIMAP *)
  display_minimap bsp p
