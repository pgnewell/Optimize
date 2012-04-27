(*
 * This implements the simplex algorithm as described in CLRS
 * variables used generally follow this pattern:
 *       _N - from CLRS N, the non-basic vector. Here it is a list
 *            of the indices to the large matrix that represent the
 *             (int list)
 *       _B - the basic indices (int list)
 *       _A - the constraint matrix
 *       b  - the constraint vector
 *       c  - the objective vector (sum c(i)*x(i) = objective)
 *            (coefficients to objective function)
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

type lp = (* A,b,c - as in CLRS *)
    float matrix * float vector * float vector

type slack_lp = (* N,B,A,b,c - as in CLRS *)
    int list * int list * float matrix * float vector * float vector * float

let init_slack (p:lp) = 
  let (_A,b,c) = p in
  let m,n = dim _A in
  let _A' = magnify (-1.) _A in 
  let _A',b,c = lower_corner _A' b c in
  let _A' = Array.append [|b|] (transpose _A') in
  let _A' = Array.append [|(Array.append [|0.|] c)|] (transpose _A') in
  (1--m,(m+1)--(m+n),_A',[||],[||],0.)

let new_pivot (slp:slack_lp) l e = 
  let _N, _B, _A, _, _, _ = slp in
  let _N = List.filter ((!=) e) _N in
  _A.(l).(l) <- 1. ; 
  let _N = l::_N in
  let _A' = extract _A _B _N in
  let _r = extract [|_A.(l)|] [0] _N in
  let _c = extract [|column e _A|] _B [0] in
  let _r = magnify (1. /. _A.(l).(e)) _c in
  _r,_A

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
    (* _B and _N must be a partition a list from 1..N *)
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
  let d = (List.length _N) + (List.length _B) in
  let _A' = make d d 0. in
  let b' = Array.create d 0. in
  let c' = Array.create d 0. in
  let _N' = List.filter ((!=) e) _N in
  let _B' = List.filter ((!=) l) _B in
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
  let v' = v +. c.(e) *. b'.(e) in
  let _ = List.iter (fun j -> c'.(j) <- c.(j) -. c.(e) *. _A'.(e).(j)) _N' in
  let _N' = List.sort (compare) (l :: _N') in
  let _B' = List.sort (compare) (e :: _B') in
  let _ = c'.(l) <- (-. ( c.(e) *. _A'.(e).(l) )) in
  (_N', _B', _A', b', c', v')

let initialize_simplex (_A:float matrix) (b:float vector) (c:float vector) :
    int list * (* N *)
    int list * (* B *)
    float matrix * (* A *)
    float array * (* b *)
    float array * (* c *)
    float = (* v *)
  let m,n = dim _A in
  let _A = magnify (-1.) _A in
  let _A',b',c' = lower_corner _A b c in
  let _N,_B = 0--(n-1),n--(n+m-1) in
  (_N,_B,_A',b',c',0.)

let positize v = Array.map (fun x -> if x <= 0. then infinity else x) v

(*
 * in pivot the leaving variable is the one that will go from basic to 
 * non basic.
 *)
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

(* 
 * determine entering and leaving indices and execute pivot on that
 * until c(_N) has no more positive values
 *)
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
                      4.; 1.; 2.; ] 
let b = [| 30.; 24.; 36. |] 
let c = [| 3.; 1.; 2. |] 

;;in
simplex _A b c 
;;

(* doesn't work *)

let _A = create (3,3) [1.; 1.; -1.; 
                      -1.; -1.; 1.; 
                      1.; -2.; 2.; ] in 
let b = [| 7.; -7.; 4. |] in
let c = [| 2.; -3.; 3. |] in
simplex _A b c 
;;

let _A = create (2,3) [-2.; -7.5; -3.; -20.; -5.; -10.] in
let c = [|-1.; -1.; -1.|] in
let b = [|10000.; 30000.|] in
simplex _A b c 
;;
