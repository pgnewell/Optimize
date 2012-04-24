
(* 
 * 
 * 
 *)
open Reader;;
open List

let read_string_list (ch:in_channel) : string list option = 
  try Some (Str.split (Str.regexp_string " ") (input_line ch))
  with _ -> None

  type charactistic = RoR of float | Beta of float | Risk of float

  type instrument = {name:string; properties:charactistic list}

  let char_of_list cl = match cl with
      "-RoR"::f -> RoR(float_of_string (hd f))
    | "-Beta"::f -> Beta(float_of_string (hd f))
    | "-Risk"::f -> Risk(float_of_string (hd f))
    | s::f -> raise (Failure ("bad charactistic " ^ s))
    | _ -> raise (Failure ("really bad charactistic "))

  let rec read_instrument ch nm : instrument =
    match read_string_list ch with
        None | Some [] -> {name=""; properties=[]}
      | Some(p':string list) -> 
        let {name=_; properties=p} = read_instrument ch "" in
        {name=nm; properties=((char_of_list p')::p)}

  let rec print_instrument ins = 
    Printf.printf "name %s " ins.name ; 
    List.iter (fun c -> print_string  
      (match c with 
          RoR f -> Printf.sprintf "RoR %f;" f
        | Risk f -> Printf.sprintf "Risk %f;" f
        | Beta f -> Printf.sprintf "Beta %f;" f))
      ins.properties

