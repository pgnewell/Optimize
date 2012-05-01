open Matrix;;
open Helpers;;
open Simplex;;
open Instrument;;

let _A = create (3,3) [1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.] in
assert (_A = [|[|1.; 2.; 3.|]; [|4.; 5.; 6.|]; [|7.; 8.; 9.|]|]) ;
assert ((extract _A [0] (0--2)) = [|[|1.; 2.; 3.|]|]) ;
assert ((extract _A [0] [0]) = [|[|1.|]|]) ;
assert ((extract _A (0--2) [0]) = [|[|1.|]; [|4.|]; [|7.|]|]) ;
assert ((transpose _A) = [|[|1.; 4.; 7.|]; [|2.; 5.; 8.|]; [|3.; 6.; 9.|]|]) ;
assert ((column 1 _A) = [|2.; 5.; 8.|]) ;
assert ((row 1 _A) = [|4.; 5.; 6.|]) ;
assert ((Helpers.replace_element 1 20 [1; 2; 3; 4; 5]) = [2; 3; 4; 5; 20]) ;
assert ((Helpers.replace_element 1 1 [1; 2; 3; 4; 5]) = [1; 2; 3; 4; 5]) ;
assert ((Helpers.replace_element 1 20 []) = [20]) ;
assert ((Helpers.replace_element 1 2 [1; 2; 3; 4; 5]) = [2; 2; 3; 4; 5]) ;

let _A' = create (6,6) 
    (let stub = [0.;0.;0.] in
    let rest1 = [1.;2.;3.] @ stub in
    let rest2 = [4.;5.;6.] @ stub in
    let rest3 = [7.;8.;9.] @ stub in
    ((Helpers.replicate 18 0.) @ rest1 @ rest2 @ rest3)) in
assert ((lower_corner _A [|1.;2.;3.|] [|4.;5.;6.;|]) = 
    (_A',[|0.; 0.; 0.; 1.; 2.; 3.|],[|4.; 5.; 6.; 0.; 0.; 0.|]))

;;

let _A = create (3,3) [1.; 1.; -1.; 
                      -1.; -1.; 1.; 
                      1.; -2.; 2.; ] in 
let b = [| 7.; -7.; 4. |] in
let c = [| 2.; -3.; 3. |] in
let result = ([0; 1; 2], [3; 4; 5],
 [|[|0.; 0.; 0.; 0.; 0.; 0.|]; 
   [|0.; 0.; 0.; 0.; 0.; 0.|];
   [|0.; 0.; 0.; 0.; 0.; 0.|]; 
   [|-1.; -1.; 1.; 0.; 0.; 0.|];
   [|1.; 1.; -1.; 0.; 0.; 0.|]; 
   [|-1.; 2.; -2.; 0.; 0.; 0.|]|],
 [|0.; 0.; 0.; 7.; -7.; 4.|], 
 [|2.; -3.; 3.; 0.; 0.; 0.|], 
 0.) in
assert(( initialize_simplex _A b c ) = result) 
;;


(*
let _A = create (2,3) [-2.; -7.5; -3.; -20.; -5.; -10.] in
let c = [|-1.; -1.; -1.|] in
let b = [|10000.; 30000.|] in
*)

