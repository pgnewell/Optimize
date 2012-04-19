open Matrix

let a = create (3,3) [-1.; -1.; -3.; 
                      -2.; -2.; -5.; 
                      -4.; -1.; -2.; ]

let b = [| 30.; 24.; 36. |]

let c = [| 3.; 1.; 2. |]

let a,b,c = lower_corner a b c

let n1 = [ 0; 1; 2; ]

let b1 = [3; 4; 5]

let select m l f = 
  let (v,i) = List.fold_left (fun p i -> 
    let v',i' = p in if (f m.(i) v') then m.(i),i else p) 
    (m.(List.hd l),(List.hd l)) l in
  v,i

let pivot 
    (* b and n must be a partition a list from 1..N *)
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

;;
let (n1', b1', a', b', c', v') = pivot n1 b1 a b c 0. 5 0;;

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

(*
  let simplex (a:float matrix) (b:float vector) (c:float vector) = 
  let nonbasic, basic, a, b, c, v = initialize_simplex in
  let x = Array.make n 0 in
(x)
*)
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
      then raise (Failure "Program is unbounded")
      else iterate_pivot (pivot nonbasic basic a b c v e l)
