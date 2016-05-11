open Segment

type t = E | N of Segment.t * t * t 

let rec parse f bsp p =
  match bsp with
  | E -> ()
  | N(root, left, right) ->
     match Segment.get_position p root with
     | L |  C -> parse f left p; f root; parse f right p
     | R -> parse f right p; f root; parse f left p

let rec rev_parse f bsp p =
  match bsp with
  | E -> ()
  | N(root, left, right) ->
     match Segment.get_position p root with
     | L |  C -> rev_parse f right p; f root; rev_parse f left p
     | R ->  rev_parse f left p; f root; rev_parse f right p

let rec iter f bsp =
  match bsp with
  | E -> ()
  | N(root, left, right) -> iter f left; f root; iter f right

let rec build_bsp sl =
  match sl with
  | [] -> E
  | s :: e -> let l, r = Segment.split s e in N(s, build_bsp l, build_bsp r)
