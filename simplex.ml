(*
 * This implements the simplex algorithm as described in CLRS
 * variables used generally follow this pattern:
 *       _N - from CLRS N, the non-basic vector. Here it is a list
 *            of the indices to the large matrix that represent the
 *            x variables in the constaints (int list)
 *       _B - the basic indices (int list)
 *       _A - the constraint matrix
 *       b  - the constraint vector
 *       c  - the objective vector (sum c(i)*x(i) = objective)
 *       v  - the slack value
 *       e  - the entering value (int)
 *       l  - the leaving value (int)
 *)

open Matrix;;
open Printf

let pf = printf
let ps = print_string
let pnl = print_newline
let pff a = Array.iter (pf "%f ") a ; pnl ()
let plst a = List.iter (pf "%d ") a ; pnl ()

(* 
 * select takes a vector m and a list of indices into that matrix, l
 * and f, a function. Returns the f'est thing in the list along with its
 * index into the vector (if f is (<) f'est is smallest)
 *)
let select m l f = 
  let (v,i) = List.fold_left (fun p i -> 
    let v',i' = p in if (f m.(i) v') then m.(i),i else p) 
    (m.(List.hd l),(List.hd l)) l in
  v,i

(* run the pivot logic as in CLRS *)
let pivot 
    (* basic and _N must be a partition a list from 1..N *)
    (_N: int list)
    (_B: int list)
    (_A: float matrix) 
    (b:float vector) 
    (c:float vector)
    (v:float) (l:int) (e:int) : 
    int list * (* N *)
    int list * (* B *)
    float matrix * (* A *)
    float vector * (* b *)
    float vector * (* c *)
    float = (* v *)
  ps "starting\n" ;
  let d = (List.length _N) + (List.length _B) in
  let _A' = make d d 0. in
  let b' = Array.create d 0. in
  let c' = Array.create d 0. in
  let _N' = List.filter ((!=) e) _N in
  let _B' = List.filter ((!=) l) _B in
  ps "checkpoint 1\n" ;
  let _ = 
    b'.(e) <- b.(l) /. _A.(l).(e) ; 
    List.iter (fun j -> _A'.(e).(j) <- _A.(l).(j) /. _A.(l).(e)) _N' ;
    _A'.(e).(l) <- 1. /. _A.(l).(e) ;
    List.iter (fun i -> b'.(i) <- b.(i) -. (_A.(i).(e) *. b'.(e)) ; 
      List.iter 
        (fun j -> _A'.(i).(j) <- _A.(i).(j) -. (_A.(i).(e) *. _A'.(e).(j))) 
        _N' ;
      _A'.(i).(l) <- -.(_A.(i).(e) *. _A'.(e).(l))
    ) _B' in
  ps "checkpoint 1\n" ;
  let v' = v +. c.(e) *. b'.(e) in
  let _ = List.iter (fun j -> c'.(j) <- c.(j) -. c.(e) *. _A'.(e).(j)) _N' in
  let _N' = List.sort (compare) (l :: _N') in
  let _B' = List.sort (compare) (e :: _B') in
  let _ = c'.(l) <- (-. ( c.(e) *. _A'.(e).(l) )) in
(*  ps "A\n" ; print_matrix _A' ;
  ps "b\n" ; plst _B' ;
  ps "c\n" ; plst _N' ; *)
  (_N', _B', _A', b', c', v')

let initialize_simplex (_A:float matrix) (b:float vector) (c:float vector) :
    int list * (* N *)
    int list * (* B *)
    float matrix * (* A *)
    float array * (* b *)
    float array * (* c *)
    float = (* v *)
  let _A',b',c' = lower_corner _A b c in
  let m,n = dim _A in
  let _N,_B = 0--(n-1),n--(n+m-1) in
  (_N,_B,_A',b',c',0.)

let rec iterate_pivot (
    (_N: int list),
    (_B: int list),
    (a: float matrix),
    (b:float vector),
    (c:float vector),
    (v:float)) : int list (* _B *) * float array (* b *) = 

  print_matrix a ;
  print_vector b ;
  print_vector c ;
  let lst = List.filter (fun j ->  c.(j) > 0.) _N in
  match lst with
      [] -> _B,b (* done, return results *)
    | _ -> (* not done so find l and do funny recursive call *)
      let e = List.hd lst in
      let delta = Array.mapi (fun i x -> 
        let y = x /. a.(i).(e) in
        if y < 0. then infinity else y) b in
      let (d,l) = select delta _B (<) in
      if d = infinity 
      then 
        let _ = pff delta in let a' = transpose a in
        let _ = pf "e = %d, a(e) = " e; pff a'.(e) in [],[||]
      (* raise (Failure "Program is unbounded") *)
      else iterate_pivot (pivot _N _B a b c v e l)

let positize v = Array.map (fun x -> if x <= 0. then infinity else x) v

let leaving _A _B b e = 
  let delta = positize (divv b (transpose _A).(e)) in
  select delta _B (<)

(* 
 * According to the text any c(i) > 0 will do but the examples always use 
 * the largest, so we do that here. None means you are done with the algorithm
*)
let entering _N c =
  let _N' = List.filter (fun j ->  c.(j) > 0.) _N in
  match _N' with [] -> None | _ ->
    let c' = (extract [|c|] [0] _N).(0) in
    let c'' = List.combine _N (Array.to_list c') in
    let e,_ = List.fold_left 
      (fun (x,y) (x',y') -> if y>y' then (x,y) else (x',y')) (-1,-1.) c'' in 
    Some e
                                                                          
(* this is nonsense but it was a lot of work *)
let rec tightest _N _A b c mo =
  match _N with
      [] -> mo
    | h::t -> 
      if c.(h) <= 0.
      then tightest t _A b c mo
      else let m = sumv (divv b (column h _A)) in
           let mo' = match mo with
               None -> Some m | Some m' -> Some (max m' m) in
           tightest t _A b c mo'

let rec iterate_pivot (_N,_B,_A,b,c,v) = 
  match entering _N c with
      None -> b,v
    | Some e -> 
      let _v,l = leaving _A _B b e in
      if _v = infinity 
      then raise (Failure "program is unbounded")
      else (
        ps "N : " ; plst _N ; pnl (); 
        ps "B : " ; plst _B ; pnl () ; 
        ps "A :\n" ; print_matrix _A ; pnl () ;
        ps "b : " ; print_vector b ; pnl () ; 
        ps "c : " ; print_vector c ; pnl () ;
        pf "v %f l %d e %d\n" v l e ;
        iterate_pivot (pivot _N _B _A b c v l e))

(* *)
let simplex (_A:float matrix) (b:float vector) (c:float vector) = 
  let _N, _B, _A, b, c, v = initialize_simplex _A b c in
  let b',v' = iterate_pivot (_N, _B, _A, b, c, v) in
  (extract [|b'|] [0] _N).(0), v'
(* *)

;;

let _A = create (3,3) [1.; 1.; 3.; 
                      2.; 2.; 5.; 
                      4.; 1.; 2.; ] in

let b = [| 30.; 24.; 36. |] in

let c = [| 3.; 1.; 2. |] in

simplex _A b c 
;;
let _A = create (3,3) [-1.; -1.; 1.; 
                      1.; 1.; -1.; 
                      -1.; 2.; -2.; ] in

let b = [| 7.; -7.; 4. |] in

let c = [| 2.; -3.; 3. |] in

simplex _A b c 
;;
