(* a module to deal with two dimensional matrices. not all functions are 
 * actually used in the project 
 * this probably won't ever work for 'a anything but float but I don't really
 * need anything else here
 *)
open Helpers;;

type 'a vector = 'a array

type 'a matrix = 'a vector array

(* a useful range function (not necessarily used anywhere *)
let (--) n m = 
  let rec rrange m' lst = 
    if m' < n 
    then lst 
    else (rrange (m' - 1) (m'::lst)) in
  rrange m []

let addv (v1:'a vector) (v2:'a vector) : 'a vector = 
  Array.mapi (fun i a -> v1.(i) +. a) v2

let prodv v1 v2 filter = Array.mapi (fun i a -> v1.(i) *. a) v2

let dot_prodv v1 v2 = Array.fold_left (+.)
  0. (Array.mapi (fun i a -> v1.(i) *. a) v2)

let sumv v = Array.fold_right (+.) v 0.

let divv v1 v2 = Array.mapi (fun i a -> v1.(i) /. a) v2

let add m1 m2 = Array.mapi (fun i a -> addv m1.(i) a) m2

let magnifyv x v = Array.map (( *. ) x) v
(*
 * fold left for f and filter i return (i * value) option
 *)
let foldl_i f a filter = 
    let l = List.combine (0--((Array.length a)-1)) (Array.to_list a) in
    let l' = List.filter (fun (i,x) -> filter i) l in
    let po = List.fold_left 
      (fun p (i,x) -> 
        match p with None -> Some (i,x)
          | Some (i',x') -> if (f x x') then Some (i,x) else Some (i',x'))
      None l' in po

let max_i a filter = foldl_i (>) a filter

let min_i a filter = foldl_i (<) a filter

let transpose m = 
  Array.mapi (fun j e -> 
    Array.mapi (fun i v -> v.(j)) m
  ) m.(1)
    
let product m1 m2 = 
  let t = transpose m2 in
  Array.map (fun v1 -> 
    Array.map (fun v2 -> dot_prodv v1 v2) t
  ) m1

let magnify x m = 
  Array.map (fun v -> 
    Array.map (fun e -> e *. x) v
  ) m

let rec identity n = 
  if n = 0 
  then [||] 
  else 
    let _I = identity (n-1) in
    let _I = Array.map (Array.append [|0.|]) _I in
    let i = Array.append [|1.|] (Array.make (n-1) 0.) in
    Array.append [|i|] _I

(*
 * Create a new matrix of dimension mxn and populate it with a list
 * (hopefully of length mxn.
 *)
let rec create ((m:int),(n:int)) (lst:'a list) : 'a matrix = 
  let rec take n lst = 
    if n = 0 then [], lst
    else match lst with 
        [] -> let took,remains = take (n-1) [] in 0.::took, []
      | h::t -> let took,remains = take (n-1) t in h::took, remains in
  let l,r = take n lst in 
  let v = Array.of_list l in
  if m = 0 then [||] else Array.append [|v|] (create (m-1,n) r)

let make (m:int) (n:int) (x:'a) : 'a matrix = Array.make_matrix m n x

(* return the dimensions of the matrix *)
let dim _M = (Array.length _M, Array.length _M.(0))

(* take matrix A and vectors b c (according to input) and make them usable 
 * by the algorithm. This means making an m+n square martix and locating the
 * original A in the lower left corner. The b and c vectors are augmented 
 * with zeroes and become m+n vectors. This allows to simply pivot the 
 * matrix in place.
 * the _X part of this can be done simply with expand
 *           expand _X 0--(m-1) m--(m+n-1)
 * but i think this would be faster
 *)
let lower_corner _X b c = 
  let m,n = dim _X in
  let stubm = Array.make m 0. in
  let stubn = Array.make n 0. in
  let b' = Array.append stubn b in
  let c' = Array.append c stubm in
  let stubmn = Array.append stubm stubn in 
  let _M = Array.make n stubmn in
  let _M' = Array.map (fun a -> Array.append a stubm) _X in
  (Array.append _M _M'),b',c'
(*
 * take a small matrix and place its values in the vertical and 
 * horizontal indices given
 *)
let expand _M verl horl = 
  let m,n = (List.fold_right (max) verl 0),(List.fold_right (max) horl 0) in
  let ms,ns = dim _M in
  let m' = (max m n) + 1 in
  let t = Helpers.cross verl horl in
  let s = Helpers.cross (0--(ms-1)) (0--(ns-1)) in
  let _M' = make m' m' 0. in
    (* assume that cross returns sorted list though not in the spec *)
  List.iter (fun ((x,y),(x',y')) -> _M'.(x').(y') <- _M.(x).(y))
    (List.combine s t) ; _M'
(* 
 * reverses the expand operation
 *)
let extract m verl horl = 
  let x = Helpers.cross verl horl in
  create ((List.length verl),(List.length horl))
    (List.map (fun (x,y) -> m.(x).(y)) x)

(*
 * extract one column as vector
 *)
let column n _M = (transpose _M).(n)
(*
 * extract one row
 *)
let row n _M = _M.(n)

  (* for testing *)
let print_vector v = 
  Array.iter (Printf.printf "%f ") v ; print_newline ()
    
let print_matrix m = Array.iter (print_vector) m

let test_extract _M verl horl result = 
  assert((extract _M verl horl) = result)

let test_column _M n result = 
  assert((column _M n) = result)

let test_row _M n result = 
  assert((row _M n) = result)

let test_expand _M verl horl result = 
  assert((expand _M verl horl) = result)

