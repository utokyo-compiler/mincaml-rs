let rec f x = x in
let x = if 0 = 1 then f 2 else f 3 in
print_int x;
print_newline ()
