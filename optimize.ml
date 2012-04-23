open Reader;;
open Matrix;;
open Simplex;;

print_string "oops" ;

let ch = open_in "model.txt" in
let m,_ = read_matrix ch in
Matrix.print_matrix m ;
close_in ch


