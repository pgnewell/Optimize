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
open Helpers;;
open Printf;;

let pf = printf

let ps = print_string

let pnl = print_newline

let pv = print_vector

let pff a = Array.iter (pf "%f ") a ; pnl ()

let plst a = List.iter (pf "%d ") a ; pnl ()

type lp = (* A,b,c - as in CLRS *)
    float matrix * float vector * float vector

type slack_lp = (* N,B,A,b,c - as in CLRS *)
    int list * int list * float matrix * float vector * float vector * float

let print_slack (slp:slack_lp) = 
  let _N, _B, _A, b, c, v = slp in
  ps "_N: " ; plst _N ;
  ps "_B: " ; plst _B ;
  ps "_A:\n" ; print_matrix _A ;
  ps "b: " ; print_vector b ;
  ps "c: " ; print_vector c ;
  printf "v: %f" v 

let init_slack (p:lp) : slack_lp = 
  let (_A,b,c) = p in
  let m,n = dim _A in
  let _A',b',c' = lower_corner _A b c in
  (0--(n-1),n--(m+n-1),_A',b',c',0.)

(* a more functional version of pivot *)
let new_pivot (slp:slack_lp) l e = 
  let _N, _B, _A, b, c, v = slp in
  let _N' = Helpers.replace_element e l _N in
  let _B' = Helpers.replace_element l e _B in
  let _r = row l _A in _r.(l) <- 1. ;
  let _c = column e _A in _c.(e) <- -1. ;
  let _r = magnifyv (1. /. _r.(e)) _r in
  let _A' = Array.mapi (fun i v -> addv (magnifyv (-._c.(i)) _r) v) _A in
  _A'.(e).(e) <- 0. ;
  let b'' = b.(l) /. _A.(l).(e) in
  let b' = subtractv b (magnifyv b'' _c) in b'.(e) <- b'' ;
  let c' = subtractv c (magnifyv c.(e) (row e _A')) in
  c'.(l) <- -.c.(e) *. _A'.(e).(l) ; c'.(e) <- 0. ;
  let v' = v +. c.(e) *. b'.(e) in
  (_N',_B',_A',b',c',v')

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

(* run the pivot logic as in CLRS *)
let old_pivot 
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

(* 
 * determine entering and leaving indices and execute pivot on that
 * until c(_N) has no more positive values
 *)
let rec iterate_pivot (slp:slack_lp) = 
  let _N,_B,_A,b,c,v = slp in 
  match entering _N c with
      None -> slp
    | Some e -> 
      let _v,l = leaving _A _B b e in
      if _v = infinity 
      then raise (Failure "program is unbounded")
      else iterate_pivot (new_pivot slp l e)

(*
 * Find a soluble initial slack form if possible (following CLRS)
 *)
let initialize_simplex (_A:float matrix) (b:float vector) (c:float vector) :
    slack_lp = 
  let slp = init_slack (_A,b,c) in
  let m,n = dim _A in
  match min_i b (fun x -> true) with
      None -> raise (Failure "initialize called with empty list")
    | Some (k,bk) -> if bk >= 0. then slp
      else
	let (_N,_B,_A,b,c,_) = slp in
	(* add an -x0 to the slack array *)
	let _A' = Array.append [|Array.make (m+n) 0.|]
	    (Matrix.transpose 
               (Array.append [|Array.make (m+n) (-1.)|] (Matrix.transpose _A))
	    ) in
	(* adjust the basic non basic lists *)
	let _N' = List.map ((+) 1) _N in
	let _B' = List.map ((+) 1) _B in
	(* the new objective function to maximize is -x0 *)
	let c' = Array.append [|-1.|] (Array.make m 0.) in
	let b' = Array.append [|0.|] b in
	(* x0 is non-basic *)
	let _N' = 0::_N' in 
	let l,e = n+k,0 in 
	let slp' = new_pivot (_N',_B',_A',b',c',0.) l e in
	let slp'' = iterate_pivot slp' in
	let _,_B'',_,_,c'',_ = slp'' in
	if c''.(0) = 0. 
	then 
	  let eo = min_i b (fun i -> List.mem i _B) in
	  let e = match eo with 
	      None -> raise (Failure "oops") 
	    | Some (e,v) -> e in
	  let l = 0 in
	  let slp''' = 
	    (* if x0 is basic do a degenerate pivot (objective unchanged) *)
	    if List.mem 0 _B 
	    then new_pivot slp'' l e 
	    else slp'' in
	  let _N',_B',_A',_,c'',v' = slp''' in
	  (* 
	   * take the original basic variables out of the new matrix 
	   * and add to the objective
	   *)
	  let _A'' = extract _A' _B (1--(m+n)) in
	  let c'' = List.fold_left 
	    (fun u i -> addv u (magnifyv b.(i) _A''.(i))) c _B in
	  let _A'' = extract _A' (1--(m+n)) (1--(m+n)) in
	  _N',_B',_A'',b',c'',v'
	else raise (Failure "infeasible")

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

(* *)
let simplex (_A:float matrix) (b:float vector) (c:float vector) = 
  let slp = init_slack (_A,b,c) in
  let _,_,_,b',_,v' = iterate_pivot slp in
  let _N,_,_,_,_,_ = slp in
  (extract [|b'|] [0] _N).(0), v'
(* *)


;;
(*

let _A = create (3,3) [1.; 1.; 3.; 
                      2.; 2.; 5.; 
                      4.; 1.; 2.; ] in
let b = [| 30.; 24.; 36. |] in
let c = [| 3.; 1.; 2. |] in
simplex _A b c
;;
let slp = init_slack (_A,b,c) in
let _N,_B,_A,b,c,v = slp in
ps "entering:" ; plst _N ; print_vector c ; pnl () ;
let e = entering _N c in
match e with
    None -> pf "Done\n"
  | Some e' -> 
    ps "leaving\n" ; print_matrix _A ; plst _B;print_vector b;pf " e %d\n" e' ;
    let v,l = leaving _A _B b e' in
    pf "l = %d e = %d\n" l e' ; 
    if v = infinity then raise (Failure "not bounded") 
    else ps "pivot\n" ; let new_slp = new_pivot slp l e' in
	 print_slack new_slp


;;
with x -> Printexc.print_backtrace stdout
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
*)
