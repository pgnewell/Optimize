open Matrix;;
open Printf

let pf = printf

let print_f_array a = 
  for i = 1 to Array.length a do
    pf "%f " a.(i-1)
  done;
  pf "\n%!"

let pff = print_f_array
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
    (* basic and nonbasic must be a partition a list from 1..N *)
    (nonbasic: int list)
    (basic: int list)
    (a: float matrix) 
    (b:float vector) 
    (c:float vector)
    (v:float) (l:int) (e:int) : 
    int list * (* N *)
    int list * (* B *)
    float matrix * (* A *)
    float array * (* b *)
    float array * (* c *)
    float = (* v *)
  let d = (List.length nonbasic) + (List.length basic) in
  let a' = make d d 0. in
  let b' = Array.create d 0. in
  let c' = Array.create d 0. in
  let _ = 
    b'.(e) <- b.(l) /. a.(l).(e) ; 
    List.iter (fun j -> 
      if j <> e then a'.(e).(j) <- a.(l).(j) /. a.(l).(e)) nonbasic ;
    a'.(e).(l) <- 1. /. a.(l).(e) ;
    List.iter (fun i -> 
      if i <> l then b'.(i) <- b.(i) -. a.(i).(e) *. b'.(e) ; 
      List.iter (fun j -> 
        if j <> e then a'.(i).(j) <- a'.(i).(j) -. a.(i).(e) *. a'.(e).(j) ;
      ) nonbasic ;
      a'.(i).(l) <- a.(i).(e) *. a'.(e).(l) 
    ) basic in
  let v' = v +. c.(e) *. b'.(e) in
  let _ = List.iter (fun j ->
    c'.(j) <- c.(j) -. c.(e) *. a'.(e).(j)
  ) nonbasic ; c'.(l) <- -.c.(e) *. a'.(e).(l) in
  let nonbasic' = l :: (List.filter ((!=) e) nonbasic) in
  let basic'    = e :: (List.filter ((!=) l) basic) in
  (nonbasic', basic', a', b', c', v')

let initialize_simplex (a:float matrix) (b:float vector) (c:float vector) :
    int list * (* N *)
    int list * (* B *)
    float matrix * (* A *)
    float array * (* b *)
    float array * (* c *)
    float = (* v *)
  let a',b',c' = lower_corner a b c in
  let m,n = range a in
  let nonbasic,basic = 0--(n-1),n--(n+m-1) in
  (nonbasic,basic,a',b',c',0.)

let rec iterate_pivot (
    (nonbasic: int list),
    (basic: int list),
    (a: float matrix),
    (b:float vector),
    (c:float vector),
    (v:float)) : int list (* basic *) * float array (* b *) = 

  let lst = List.filter (fun j ->  c.(j) > 0.) nonbasic in
  match lst with
      [] -> basic,b (* done, return results *)
    | _ -> (* not done so find l and do funny recursive call *)
      let e = List.hd lst in
      let delta = Array.mapi (fun i x -> 
        let y = x /. a.(i).(e) in
        if y < 0. then infinity else y) b in
      let (d,l) = select delta basic (<) in
      if d = infinity 
      then 
        let _ = pff delta in let a' = transpose a in
        let _ = pf "e = %d, a(e) = " e; pff a'.(e) in [],[||]
      (* raise (Failure "Program is unbounded") *)
      else iterate_pivot (pivot nonbasic basic a b c v e l)
;;

(* *)
let simplex (a:float matrix) (b:float vector) (c:float vector) = 
  let nonbasic, basic, a, b, c, v = initialize_simplex a b c in
  let basic,x = iterate_pivot (nonbasic, basic, a, b, c, v) in
basic, x
(* *)

let a = create (3,3) [1.; 1.; 3.; 
                      2.; 2.; 5.; 
                      4.; 1.; 2.; ]

let b = [| 30.; 24.; 36. |]

let c = [| 3.; 1.; 2. |]

;;
simplex a b c 
;;
