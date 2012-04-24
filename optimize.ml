open Reader;;
open Matrix;;
open Simplex;;
open Instrument;;

let ch = open_in "model.txt" in
let instl,m,o = read_model ch in
List.iter (Instrument.print) instl ;
print_matrix m ;
close_in ch



