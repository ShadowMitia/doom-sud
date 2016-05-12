open Point
open Segment
open Bsp
open Player
open Graphics

let clipping bsp =
  failwith "TODO"

let display_minimap bsp p =
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 200 200;
  Graphics.set_color Graphics.red;
  Bsp.iter (fun seg -> Graphics.draw_segments [|seg.porig.x / Options.scale, seg.porig.y / Options.scale, seg.pdest.x / Options.scale, seg.pdest.y / Options.scale|]) bsp;
  Graphics.set_color Graphics.green;
  Graphics.fill_circle (p.pos.x / Options.scale) (p.pos.y / Options.scale) 3

let display_player player =
  Graphics.set_color Graphics.red;
  Graphics.fill_circle player.pos.x player.pos.y 10

let display bsp p =
  (* BACKGROUND *)
  Graphics.set_color Graphics.green;
  Graphics.fill_rect 0 0 Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.blue;
  Graphics.fill_rect 0 (Options.win_h / 2) Options.win_w (Options.win_h / 2);
  Graphics.set_color Graphics.black;
  (* WALLS *)
  Bsp.iter (fun seg ->
      (* Segment.print_segment seg; *)
      Graphics.draw_segments [|seg.porig.x, seg.porig.y, seg.pdest.x, seg.pdest.y |]) bsp;
  Bsp.iter (fun seg ->
           Graphics.moveto (seg.porig.x + ((seg.pdest.x - seg.porig.x) / 2))
                           (seg.porig.y + ((seg.pdest.y - seg.porig.y) / 2));
           Graphics.draw_string seg.id) bsp;
  (* PLAYER *)
  display_player p;
  display_minimap bsp p
