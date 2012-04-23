(* a module to deal with two dimensional matrices. not all functions are 
 * actually used in the project 
 * this probably won't ever work for 'a anything but float but I don't really
 * need anything else here
 *)

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

  let rec create ((n:int),(m:int)) (lst:'a list) : 'a matrix = 
    let rec take n lst = 
      if n = 0 then [], lst
      else match lst with 
          [] -> let took,remains = take (n-1) [] in 0.::took, []
        | h::t -> let took,remains = take (n-1) t in h::took, remains in
    let l,r = take n lst in 
    let v = Array.of_list l in
    if m = 0 then [||] else Array.append [|v|] (create (n,m-1) r)

  let make (m:int) (n:int) (x:'a) : 'a matrix = Array.make_matrix m n x

(* return the dimensions of the matrix *)
  let range m = (Array.length m, Array.length m.(0))

(* a useful range function (not necessarily used anywhere *)
  let (--) n m = 
    let rec rrange m' lst = 
      if m' < n 
      then lst 
      else (rrange (m' - 1) (m'::lst)) in
    rrange m []

(* take arrays A b c (according to input) and make them usable by the algorithm
 * this means making an m+n square martix and locating the original a in the
 * lower left corner. The b and c arrays are augmented with zeroes and become
 * m+n vectors *)
  let lower_corner x b c = 
    let m,n = range x in
    let stubm = Array.make m 0. in
    let stubn = Array.make n 0. in
    let stubmn = Array.append stubm stubn in 
    let b' = Array.append stubn b in
    let c' = Array.append c stubm in
    let rec makematrix m' n' = 
      if m' > 0 
      then Array.append [|stubmn|] (makematrix (m' - 1) n')
      else 
        if n' > 0 
        then (Array.append 
          [|(Array.append x.(n - n') stubm)|] (makematrix m' (n' - 1)))
        else [||] in
    makematrix m n, b', c'

  let print_matrix m = 
    for i = 0 to (Array.length m) -1 do
      Array.iter (Printf.printf "%f ") m.(i) ; print_newline ()
    done


        
    
