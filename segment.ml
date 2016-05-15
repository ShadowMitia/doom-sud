open Point

type t = {id : string;
          porig : Point.t;
          pdest : Point.t;
          ci : float; (* real begin of segment in % *)
          ce : float; (* real end of end in % *)
         }

type tpos = L | R | C

let get_id_counter i =
  let id = ref i in
  fun () -> id := !id + 1; string_of_int !id

let get_id = get_id_counter 0

let print_segment seg =
  Printf.printf "[id=%s -> xA=%d yA=%d xB=%d yB=%d]\n" seg.id seg.porig.x seg.porig.y seg.pdest.x seg.pdest.y

let new_segment ?id:(iden="") xo yo xd yd =
  let orig = Point.new_point xo yo in
  let dest = Point.new_point xd yd in
  { id=(get_id ()); porig=orig; pdest=dest; ci=0.0; ce=1.0}

let get_real_coord s =
    (* Les "vrais" valeurs des extrémités du segment*)
  let lx = s.pdest.x - s.porig.x in
  let ly = s.pdest.y - s.porig.y in
  let xo = (float_of_int s.porig.x) +. ((float_of_int lx) *. s.ci) in (* xA *)
  let xd = (float_of_int s.porig.x) +. ((float_of_int lx) *. s.ce) in (* xB *)
  let yo = (float_of_int s.porig.y) +. ((float_of_int ly) *. s.ci) in (* yA *)
  let yd = (float_of_int s.porig.y) +. ((float_of_int ly) *. s.ce) in (* yB *)
  (xo, yo, xd, yd)

let get_position p s =
  let xo, yo, xd, yd = get_real_coord s in
  (* On calcule le produit vectoriel de la troisième coordonnée entre le segment et le point relié à l'origine du segment*)
  let z = (xd -. xo) *. ((float_of_int p.y) -. yo) -. (yd -. yo) *. ((float_of_int p.y) -. xo) in
  match z with
  | z when z < 0.0 -> R (* droite *)
  | 0.0 -> C (*  Colinéaire *)
  | z  when z > 0.0 -> L (* gauche *)
  | _ -> assert false (* Tout les cas sont dans les autres pattern *)

                (*
let split_segment delim segm = (* segm est le segment à splitter par rapport à delim*)
  let xA, yA ,xB, yB = get_real_coord delim in
  let xC, yC, xD, yD = get_real_coord segm in
  let d = (xB -. xA) *. (yD -. yC) -. (yB -. yA) *. (xD -. xC) in
  let pos_C = get_position segm.porig delim in
  let pos_D = get_position segm.pdest delim in
  match d with
  | 0.0 -> (match pos_C with (* On cherche la position d'un point du segment par rapport à delim*)
         | L | C-> (Some segm, None)
         | R -> (None, Some segm))
  | _ ->(
    let z = (xB -. xA) *. (yC -. yA) -. (yB -. yA) *. (xC -. xA) in (* produit vectoriel coord z *)
    let c = ((-.z) /. d) in
    match c with
    | c when c < 1.0 && c > 0.0 ->
      (Some {segm with id=(segm.id^"left");ce=c}, Some {segm with id=(segm.id^"right");ci=c})
    | 0.0 (* C sur la droite AB *) -> (match pos_D with
             | L | C -> (Some segm, None)
             | R -> (None, Some segm))
    | 1.0 (* D sur la droite AB *)-> (match pos_C with
             | L | C -> (Some {segm with id=(segm.id^"left");ce=c}, Some {segm with id=(segm.id^"right");ci=c})
             | R -> (Some {segm with id=(segm.id^"left");ci=c}, Some {segm with id=(segm.id^"right");ce=c}))
    | _ -> (match pos_C with
           | L | C -> (Some {segm with id=(segm.id^"left");ce=c}, Some {segm with id=(segm.id^"right");ci=c})
           | R -> (Some {segm with id=(segm.id^"left");ci=c}, Some {segm with id=(segm.id^"right");ce=c})))
                 *)
let new_segment_from_L segm c =
  Some {segm with id=(get_id ());ce=c}, Some {segm with id=(get_id());ci=c}

let new_segment_from_R segm c =
  Some {segm with id=(get_id ());ci=c}, Some {segm with id=(get_id());ce=c}

let split_segment delim segm =
  let xA, yA ,xB, yB = get_real_coord delim in
  let xC, yC, xD, yD = get_real_coord segm in
  let d = (xB -. xA) *. (yD -. yC) -. (yB -. yA) *. (xD -. xC) in
  let pos_C = get_position segm.porig delim in
  let pos_D = get_position segm.pdest delim in
  if d = 0.0 then
    match pos_C with
    | L | C -> (Some segm, None)
    | R -> (None, Some segm)
  else
    let z = (xB -. xA) *. (yC -. yA) -. (yB -. yA) *. (xC -. xA) in (* produit vectoriel coord z *)
    let c = ((-.z) /. d) in
    match pos_C with
    | L -> begin match pos_D with
           | L | C -> (Some segm, None)
           | R -> new_segment_from_L segm c
           end
    | R -> begin match pos_D with
           | R | C -> (None, Some segm)
           | L -> new_segment_from_R segm c
           end
    | C -> begin match pos_D with
           | L | C -> (Some segm, None)
           | R -> (None, Some segm)
           end

let split hd rest =
  let rec split_rec hd rest list_split =
    match rest with
    | [] -> list_split
    | el :: s -> let l,r = split_segment hd el in
                 match l,r with
                 | None, None -> split_rec el s list_split
                 | Some l, None -> split_rec el s (l :: (fst list_split), (snd list_split))
                 | None, Some r -> split_rec el s ((fst list_split), r::(snd list_split))
                 | Some l, Some r -> split_rec el s (l :: (fst list_split), r :: (snd list_split))
  in split_rec hd rest ([], [])
