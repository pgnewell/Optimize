open Reader;;
open Matrix;;
open Simplex;;

let ch = open_in "model.txt" in
let l = read_string_list ch in
let nm = List.nth 1 l in
let m,_ = read_instrument nm in
Matrix.print_ma
close_in ch



