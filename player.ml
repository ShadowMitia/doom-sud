open Options
open Physic
open Point

type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

let new_player pos pa = {pos; pa}

type dir = Left | Right

let rotate d p =
  match d with
  | Left ->  p.pa <- p.pa + 25
  | Right -> p.pa <- p.pa - 25


type mv = MFwd | MBwd | MLeft | MRight

let move d p bsp =
  let p_step = (int_of_float Options.step_dist) in
  match d with
  | MFwd -> let new_y = p.pos.y + p_step in
            p.pos <- Point.new_point p.pos.x new_y
  | MBwd -> let new_y = p.pos.y - p_step in
            p.pos <- Point.new_point p.pos.x  new_y
  | MRight -> let new_x = p.pos.x + p_step in
            p.pos <- Point.new_point new_x p.pos.y
  | MLeft -> let new_x = p.pos.x - p_step in
             p.pos <- Point.new_point new_x p.pos.y
