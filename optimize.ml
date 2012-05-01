open Reader;;
open Matrix;;
open Simplex;;
open Instrument;;

let file = if Array.length Sys.argv < 2
then raise (Failure "please enter file name on command line")
else Sys.argv.(1) in
let ch = open_in file in
let _A, b, c, ia = read_model_lp ch in
let b,v = simplex _A b c in
Array.iteri (fun i v -> Printf.printf "Purchase %f of %s\n" v ia.(i)) b ;
close_in ch
