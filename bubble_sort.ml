let rec sort low high lst =
	let x = 9 (*partition low high lst*) in
	if low < x then (sort low (x-1) lst)
	else 
		if high > x then (sort (x+1) high lst)
;;

(*let qsort l = 
	let x = List.length(l)
	in sort 0 x l;;

*)


let partition low high lst =
	let 	x = List.hd lst
		i = low
	in recurse x i (List.tl lst) (low+1) high;;

let rec recurse x i lst low high =
	match lst with 
	| [] -> []
	| h :: t -> if (h <= x) then (recurse 
			else (recurse x i lst low high)	
