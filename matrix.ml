module type RING = 
sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val mul  : t -> t -> t
end


module type MATRIX = 
sig
  type 'a matrix
  val add : 'a matrix -> 'a matrix -> 'a matrix
  val product : 'a matrix -> 'a matrix -> 'a matrix
  val magnify : 'a -> 'a matrix -> 'a matrix
  val identiry : int -> 'a matrix
  val transpose : 'a matrix -> 'a matrix
end

module MATRIX = 
struct 
  type 'a vector = 'a array

  type 'a matrix = 'a vector array

  type dim = int * int

  let addv (v1:'a vector) (v2:'a vector) : 'a vector = 
    Array.mapi (fun i a -> v1.(i) +. a) v2

  let prodv v1 v2 = Array.fold_right (+.)
    (Array.mapi (fun i a -> v1.(i) *. a) v2) 0.

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

  let rec create (n,m) lst = 
    let rec take n lst = 
      if n = 0 then [], lst
      else match lst with 
          [] -> let took,remains = take (n-1) [] in 0::took, []
        | h::t -> let took,remains = take (n-1) t in h::took, remains in
    let l,r = take n lst in 
    let v = Array.of_list l in
    if m = 0 then [||] else Array.append [|v|] (create (n,m-1) r)
    
end

