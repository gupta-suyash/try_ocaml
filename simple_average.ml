(*let sum = (fun x y -> x + y) in
	(print_int ((sum 4 5) + 2));;
*)

(*let sum = (fun x y -> x + y);;*)

(*let sum x = x+2;;*)

let average a b = 
	let sum = a +. b in
	sum /. 2.0
in
print_float (average 4.2 9.2);;
