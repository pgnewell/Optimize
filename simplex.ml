open Matrix

let a = create (6,6) [ 0.;  0.;  0.; 0.; 0.; 0.; 
                       0.;  0.;  0.; 0.; 0.; 0.; 
                       0.;  0.;  0.; 0.; 0.; 0.;
                      -1.; -1.; -3.; 0.; 0.; 0.; 
                      -2.; -2.; -5.; 0.; 0.; 0.; 
                      -4.; -1.; -2.; 0.; 0.; 0.; ]

let b = [| 0.; 0.; 0.; 30.; 24.; 36. |]

let c = [| 3.; 1.; 2.; 0.; 0.; 0. |]

let n1 = [ 1; 2; 3; ]

let b1 = [3; 4; 5]

let pivot 
    (* b and n must be a partition a list from 1..N *)
    (nonbasic: int list)
    (basic: int list)
    (a: float matrix) 
    (b:float vector) 
    (c:float vector)
    (v:float) (l:int) (e:int) : 
    int list * int list * float matrix * float array * float array * float = 
  let dim = (List.length nonbasic) + (List.length basic) in
  let a' = make dim dim 0. in
  let b' = Array.create dim 0. in
  let c' = Array.create dim 0. in
  let _ = 
    b'.(e) <- b.(l) /. a.(l).(e) ; 
    List.iter (fun j -> 
      if j <> e then a.(e).(j) <- b.(l) /. a.(l).(e)) nonbasic ;
    a.(e).(l) <- 1. /. a.(l).(e) ;
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
  let nonbasic' = l :: (List.filter ((=) e) nonbasic) in
  let basic'    = e :: (List.filter ((=) l) basic) in
  (nonbasic', basic', a', b', c', v')

;;
