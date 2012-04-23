(* reader.ml
 * functions to read matrices, etc from stdin 
 * 
 *)

(*let readf : float = Scanf.scan s "%f %n" (fun s' n -> *)

let fos = float_of_string

let ios = int_of_string

let split = Str.split (Str.regexp_string " ")

let rec read vs is ch =
  match try Some(split (input_line ch)) with _ -> None with
  | None -> vs, is
  | Some[x;y;z;_;_] -> read ((fos x, fos y, fos z) :: vs) is ch
  | Some["3";i;j;k] -> read vs ((ios i, ios j, ios k) :: is) ch
  | Some s -> read vs is ch

let vertices, indices =
  let ch = open_in "model.txt" in
  let vs, is = read [] [] ch in
  close_in ch;
  Printf.printf "%d vertices, %d triangles\n%!"
    (List.length vs) (List.length is);
  Array.of_list (List.rev vs), is

let read_matrix (ch:in_channel) : (float Matrix.matrix) * Matrix.dim = 
  let rec read_numbers cols = 
    match 
      try Some (Str.split (Str.regexp_string " ") (input_line ch))
      with _ -> None with 
          None -> [||],(0,0)
        | Some [] -> [||],(0,0)
        | Some (h::t) -> 
          let c = List.length (h::t) in
          let co = match cols with 
              None -> Some c 
            | Some n -> 
              if n != c 
              then raise (Failure "unmatching rows")
              else Some c in
          let arrayf = Array.of_list (List.map (float_of_string) (h::t)) in
          let rest,(rs,cs) = read_numbers co in
          Array.append [|arrayf|] rest,(rs+1,cs) 
  in
  read_numbers None


