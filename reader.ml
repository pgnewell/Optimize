(* reader.ml
 * functions to read matrices, etc from stdin 
 * 
 *)

(*let readf : float = Scanf.scan s "%f %n" (fun s' n -> *)
open Instrument;;
open Str;;

let read_string_list (ch:in_channel) : string list option = 
  try Some (Str.split (Str.regexp_string " ") (input_line ch))
  with _ -> None

let read_constraint constraints = 
  let c = Str.split (Str.regexp_string "<") constraints in
  List.hd c, float_of_string (List.nth c 1)

let read_objective obj = 
  match obj with
      "<"::t -> -1.,List.hd t 
    | ">"::t -> 1.,List.hd t
    | _ -> Printf.printf " %s " (List.hd obj) ; 
      raise (Failure "invalid objective")
(*
let read_matrix (ch:in_channel) : (float Matrix.matrix) * (int * int) = 
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
*)
let rec read_model ch : 
    Instrument.t list * (string * float) list * (float * string) option = 
  match read_string_list ch with 
      None -> [],[],None
    | Some [] -> read_model ch
    | Some("Instrument:"::name) -> 
      let instr = Instrument.read_ins ch (List.hd name) in
      let il,constraints,obj = read_model ch in instr::il,constraints,obj
    | Some("Constraints:"::c) -> 
      let constr = List.map (fun ins -> read_constraint ins) c in
      let il,_,obj = read_model ch in il,constr,obj
    | Some("Objective:"::o) ->
      let obj = read_objective o in
      let il,model,_ = read_model ch in il,model,Some obj
    | Some (s::l) -> raise (Failure ("bad format" ^ s)) 

(* 
 * call above and process for standard linear program
 *)
let read_model_lp ch = 
  let il,cl,oo = read_model ch in
  let c = match oo with 
      None -> raise (Failure "No objective") 
    | Some (s,n) -> 
      List.map (fun ins -> 
	(Instrument.get_property ins n) *. s) il in
  let b = match cl with
      [] -> raise (Failure "No Constraints")
    | _ -> List.map (fun (cn,cv) -> cv) cl in
  let _A = List.fold_left (fun u (c,v) -> 
    Array.append u [|(Array.of_list
      (List.map (fun i -> Instrument.get_property i c) il))|] 
    ) [||] cl in
  let il = Array.of_list (List.map (fun i -> Instrument.get_name i) il) in
  _A, Array.of_list b, Array.of_list c, il
    
