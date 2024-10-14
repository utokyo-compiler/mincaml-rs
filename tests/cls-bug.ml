(* 「素朴」なknown function optimizationでは駄目な場合 *)
let rec f x = x + 123 in
let rec g y = f in
print_int ((g 456) 789)
