type t = {
  mutable pos : Point.t;
  mutable pa : int;
}

val new_player : Point.t -> int -> t

type dir = Left | Right

val rotate : dir -> t -> unit

type mv = MFwd | MBwd | MLeft | MRight

val move : mv -> t -> Bsp.t -> unit


