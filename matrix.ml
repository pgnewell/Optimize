(* a module to deal with two dimensional matrices. not all functions are 
 * actually used in the project 
 * this probably won't ever work for 'a anything but float but I don't really
 * need anything else here
 *)
open Helpers;;

type 'a vector = 'a array

type 'a matrix = 'a vector array

let addv (v1:'a vector) (v2:'a vector) : 'a vector = 
  Array.mapi (fun i a -> v1.(i) +. a) v2

let prodv v1 v2 = Array.mapi (fun i a -> v1.(i) *. a) v2

let sumv v = Array.fold_right (+.) v 0.

let divv v1 v2 = Array.mapi (fun i a -> v1.(i) /. a) v2

let add m1 m2 = Array.mapi (fun i a -> addv m1.(i) a) m2

let transpose m = 
  Array.mapi (fun j e -> 
    Array.mapi (fun i v -> v.(j)) m
  ) m.(1)
    
let product m1 m2 = 
  let t = transpose m2 in
  Array.map (fun v1 -> 
    Array.map (fun v2 -> prodv v1 v2) t
  ) m1

let magnify x m = 
  Array.map (fun v -> 
    Array.map (fun e -> e *. x) v
  ) m

let identity n = 
  let a = Array.make n 0 in
  Array.mapi 
    (fun i v -> let v' = Array.make n 0. in 
                let _ = v'.(i) <- 1. in v') a

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

(* a useful range function (not necessarily used anywhere *)
let (--) n m = 
  let rec rrange m' lst = 
    if m' < n 
    then lst 
    else (rrange (m' - 1) (m'::lst)) in
  rrange m []

(* take matrix A and vectors b c (according to input) and make them usable 
 * by the algorithm. This means making an m+n square martix and locating the
 * original A in the lower left corner. The b and c vectors are augmented 
 * with zeroes and become m+n vectors. This allows to simply pivot the 
 * matrix in place. Using the basic (vertical) and nonbasic (horizontal)
 * lists to indicate the indices of the active elements.
 * in the resulting array the current constraints can be represented, e.g.:
 * for b = [2;3;5] and c = [0;1;4] (b and c must partition 0..5)
 *      x2 = b2 + m2,0*x0 +  m2,1*x1 + m2,4*x4
 *      x3 = b2 + m3,0*x0 +  m3,1*x1 + m3,4*x4
 *      x5 = b2 + m3,0*x0 +  m3,1*x1 + m3,4*x4
 *)
let lower_corner _X b c = 
  let m,n = dim _X in
  let stubm = Array.make m 0. in
  let stubn = Array.make n 0. in
  let stubmn = Array.append stubm stubn in 
  let _M = Array.make n stubmn in
  let _M' = Array.map (fun a -> Array.append a stubm) _X in
  let b' = Array.append stubn b in
  let c' = Array.append c stubm in
  (Array.append _M _M'),b',c'

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

let extract m verl horl = 
  let x = Helpers.cross verl horl in
  create ((List.length verl),(List.length horl))
    (List.map (fun (x,y) -> m.(x).(y)) x)

let column n _M = (transpose _M).(n)

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

