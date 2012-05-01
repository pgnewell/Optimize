
(* 
 * 
 * 
 *)
open List
open Str;;

module type INSTRUMENT = 
sig
  type charactistic
  type t
  val print_ins : t -> unit
  val read_ins : in_channel -> string -> t
  val get_property : t -> string -> float
  val get_name : t -> string
end

module Instrument : INSTRUMENT = 
struct 
  let read_string_list (ch:in_channel) : string list option = 
    try Some (Str.split (Str.regexp_string " ") (input_line ch))
    with _ -> None

  type charactistic = RoR of float | Price of float | Risk of float | 
      ExpRE of float

  type t = {name:string; properties:charactistic list}

  let char_of_list cl = match cl with
      "-RoR"::f -> RoR(float_of_string (hd f))
    | "-Price"::f -> Price(float_of_string (hd f))
    | "-Risk"::f -> Risk(float_of_string (hd f))
    | "-ExpRE"::f -> ExpRE(float_of_string (hd f))
    | s::f -> raise (Failure ("bad charactistic " ^ s))
    | _ -> raise (Failure ("really bad charactistic "))

  let prop_list ins = 
    List.map (fun p -> match p with
      | RoR f -> "RoR",f
      | Price f -> "Price",f 
      | Risk f -> "Risk",f 
      | ExpRE f -> "ExpRE",f) ins.properties 

  let get_property ins name = 
      List.assoc name (prop_list ins)

  let get_name ins = ins.name

  let rec read_ins ch nm : t =
    match read_string_list ch with
        None | Some [] -> {name=""; properties=[]}
      | Some(p':string list) -> 
        let {name=_; properties=p} = read_ins ch "" in
        {name=nm; properties=((char_of_list p')::p)}

  let print_ins ins = 
    Printf.printf "name %s " ins.name ; 
    List.iter (fun c -> print_string  
      (match c with 
          RoR f -> Printf.sprintf "RoR %f;" f
        | Risk f -> Printf.sprintf "Risk %f;" f
        | Price f -> Printf.sprintf "Price %f;" f
        | ExpRE f -> Printf.sprintf "ExpRE %f;" f))
      ins.properties ; 
    print_newline ()

end
