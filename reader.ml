(* reader.ml
 * functions to read matrices, etc from stdin 
 * 
 *)

(*let readf : float = Scanf.scan s "%f %n" (fun s' n -> *)
open Instrument
open Matrix

let read_string_list (ch:in_channel) : string list option = 
  try Some (Str.split (Str.regexp_string " ") (input_line ch))
  with _ -> None

let read_matrix (ch:in_channel) : (float Matrix.matrix) * Matrix.dim = 
  let rec read_numbers cols = 
    match read_string_list ch with 
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

let rec read_model ch : Instrument.t list * float matrix * float vector  = 
  match read_string_list ch with 
      None | Some [] -> [],[||],[||]
    | Some("Instrument:"::name) -> 
      let instr = Instrument.read ch (List.hd name) in
      let il,model,obj = read_model ch in instr::il,model,obj
    | Some("Model:"::_) -> 
      let model,_ = read_matrix ch in 
      let il,model',obj = read_model ch in il,model,obj
    | Some("Objective:"::_) ->
      let m,_ = read_matrix ch in
      let obj = m.(0) in
      let il,model,obj' = read_model ch in il,model,obj
    | Some (s::l) -> raise (Failure ("bad format" ^ s))

