let rec inprod v1 = v1.(0) in
let v1 = Array.make 1 1.23 in
print_int (int_of_float (inprod v1));
print_newline ()
