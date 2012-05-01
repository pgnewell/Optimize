(** A package of generic and useful helper functions. *)
module Helpers =
struct
  (*************************)
  (***** Array Helpers *****)
  (*************************)

  (** Modify each element of an array in place *)
  let array_map_modify (f:'a -> 'a) (arr:'a array) : unit =
    Array.iteri (fun i _ -> arr.(i) <- f (arr.(i))) arr

  (************************)
  (***** List Helpers *****)
  (************************)

  (** A left fold. *)
  let rec fold_left (f:'b -> 'a -> 'b) (i:'b) (xs:'a list) : 'b =
    match xs with
    | [] -> i
    | hd::tl -> fold_left f (f i hd) tl

  (** A right fold. *)
  let rec fold_right (f:'a -> 'b -> 'b) (i:'b) (xs:'b list) : 'b =
    match xs with
    | [] -> i
    | hd::tl -> f hd (fold_right f i tl)

  (** The list containing n replicas of e *)
  let rec replicate (n:int) (e:'a) : 'a list = 
    if n = 0 then [] else e::replicate (n-1) e

  (** The cross produce of lists xs and ys. 
      result = \[ (x,y) | x in xs and y in ys \] *)
  let rec cross (xs:'a list) (ys:'b list) : ('a*'b) list = 
    match xs with
    | [] -> []
    | hd::tl -> List.map (fun y -> (hd,y)) ys @ cross tl ys

  (** The monotonically increasing list containing each number in the range
      between n1 and n2 (inclusive) *)
  let rec range (n1:int) (n2:int) : int list = 
    if n1 > n2 then [] else n1::range (n1+1) n2

  (** The list of unique elements in xs. *)
  let rec unique xs =
    match xs with
    | [] -> []
    | hd::tl ->
        let tl' = unique tl in
        if List.mem hd tl' then tl' else hd::tl'

  let print_lst p s l = print_string p ;
    List.iter (fun e -> Printf.printf s e) l ; print_newline() ; 
    flush_all()

  let replace_element take give lst = 
    List.sort (compare) (give::((List.filter ((!=) take)) lst))

  (**************************)
  (***** Option Helpers *****)
  (**************************)

  (** True if the value x is of the form (Some _) *)
  let is_some o = match o with Some _ -> true | _ -> false

  (** Force a value of type option that is known to have the form (Some x) to
      the value x. *)
  let from_some (vM:'a option) : 'a =
    match vM with
    | None -> failwith "cannot call from_some on None"
    | Some v -> v

  (**************************)
  (***** Number Helpers *****)
  (**************************)

  (** Bound x between low and high. *)
  let bound (low:int) (high:int) (x:int) : int = min (max low x) high

  (** The integer value of a boolean where true = 1 and false = 0. *)
  let int_of_bool b = if b then 1 else 0

  (********************************)
  (***** Random Value Helpers *****)
  (********************************)

  (** call f with probability (1/p) and g if f is not called *)
  let with_inv_probability_or (r:int->int) (p:int) 
                              (f:unit->'a) (g:unit->'a) : 'a =
    if r p = 0 then f () else g ()

  (** Call f with probability (1/p) (using r to generate random numbers) *)
  let with_inv_probability (r:int->int) (p:int) (f:unit->unit) : unit = 
    with_inv_probability_or r p f (fun () -> ())

  (** Call one of the functions in the list with equal probability. *)
  let with_equal_probability (r:int->int) (fs:(unit -> unit) list) : unit =
    (List.nth fs (r (List.length fs))) ()

end
