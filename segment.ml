open Point;;

type t = {id : string;
          porig : Point.t;
          pdest : Point.t;
          ci : float;
          ce : float;
         }

type tpos = L | R | C

let new_segment ?id:(iden="segment") xo yo xd yd =
  let orig = Point.new_point xo yo in
  let dest = Point.new_point xd yd in
  { id=iden; porig=orig; pdest=dest; ci=0.0; ce=1.0}

let get_real_coord s =
    (* Les "vrais" valeurs des extrémités du segment*)
  let lx = s.pdest.x - s.porig.x in
  let ly = s.pdest.y - s.porig.y in
  let xo = s.porig.x + (int_of_float ((float_of_int lx) *. s.ci)) in (* xA *)
  let xd = s.porig.x + (int_of_float ((float_of_int lx) *. s.ce)) in (* xB *)
  let yo = s.porig.y + (int_of_float ((float_of_int ly) *. s.ci)) in (* yA *)
  let yd = s.porig.y + (int_of_float ((float_of_int ly) *. s.ce)) in (* yB *)
  (xo, yo, xd, yd)

let get_position p s =
  let xo, yo, xd, yd = get_real_coord s in
  (* On calcule le produit vectoriel de la troisième coordonnée entre le segment et le point relié à l'origine du segment*)
  let z = (xd - xo) * (p.y - yo) - (yd - yo) * (p.y - xo) in
  match z with
  | z when z < 0 -> R (* droite *)
  | 0 -> C (*  Colinéaire *)
  | z  when z > 0 -> L (* gauche *)
  | _ -> assert false (* Tout les cas sont dans les autres pattern *)


(* TODO: Bug: génère des segments de tailles 0 dans certaines situations. Pas génant pour le moment *)
let split_segment delim segm = (* segm est le segment à splitter par rapport à delim*)
  let xA, yA ,xB, yB = get_real_coord segm in
  let xC, yC, xD, yD = get_real_coord delim in
  let d = (xB - xA) * (yD - yC) - (yB - yA) * (xD - xC) in
  match  d with
  | 0 -> (match get_position segm.porig delim with (* On cherche la position d'un point du segment par rapport à delim*)
         | L -> (Some segm, None)
         | R -> (None, Some segm)
         | C -> (Some segm, None) (* Par convention *))
  | _ ->
    let z = (xD - xA) * (yC - yA) - (yD - yA) * (xC - xA) in (* produit vectoriel coord z *)
    let c = (float_of_int (-z)) /. (float_of_int d) in
    match print_float c; print_newline ();c  with
    | c when c < 1.0 && c > 0.0 ->(
      let xI = xA + (int_of_float (c *. (float_of_int (xB - xA)))) in
      let yI = yA + (int_of_float (c *. (float_of_int (yB - yA)))) in
      (Some (new_segment ~id:("Intersection left " ^ segm.id) xA yA xI yI), Some(new_segment ~id:("Intersection right " ^ segm.id) xI yI xB yB))
    )
  | 0.0 -> (Some (new_segment xA yA xC yC), Some(new_segment xC yC xB yB))
  | 1.0 -> (Some(new_segment xA yA xD yC), Some(new_segment xD yC xB yB))
  | _ -> match get_position segm.porig delim with
         | L -> (Some segm, None)
         | R -> (None, Some segm)
         | C -> (Some segm, None)

let split hd rest =
  let rec split_rec hd rest list_split =
    match rest with
    | [] -> list_split
    | el :: s -> let l,r = split_segment hd el in
                 match l,r with
                 | None, None -> split_rec hd s list_split
                 | Some l, None -> split_rec hd s (l :: (fst list_split), (snd list_split))
                 | None, Some r -> split_rec hd s ((fst list_split), (snd list_split))
                 | Some l, Some r -> split_rec hd s (l :: (fst list_split), r :: (snd list_split))
  in split_rec hd rest ([], [])
