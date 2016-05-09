let read_lab ci =
  let r = Str.regexp "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
  let rp = Str.regexp "P : \\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)" in
  let rc = Str.regexp "\\\\\\.*" in
  let p, l =
    let rec rr (p, acc) =
      try
        let l = input_line ci in
        if Str.string_match rp l 0 then
          let xp = int_of_string (Str.replace_first rp "\\1" l) in
          let yp = int_of_string (Str.replace_first rp "\\2" l) in
          let ap = int_of_string (Str.replace_first rp "\\3" l) in
          rr (Some (xp, yp, ap), acc)
        else if Str.string_match rc l 0 then
          rr (p, acc)
        else
          let xo = int_of_string (Str.replace_first r "\\1" l) in
          let yo = int_of_string (Str.replace_first r "\\2" l) in
          let xd = int_of_string (Str.replace_first r "\\3" l) in
          let yd = int_of_string (Str.replace_first r "\\4" l) in
          rr (p, (xo, yo, xd, yd) :: acc)
      with End_of_file -> close_in ci; (p, acc)
    in rr (None, []) in
  let p = match p with 
    | None -> Format.eprintf "No player provided@."; raise Exit
    | Some c -> c in
  p, l
