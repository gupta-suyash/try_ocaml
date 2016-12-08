open Oml

type 'a mytree = 
| Leaf 
| Node of 'a * 'a mytree * 'a mytree;;


let tossCoin prob = 
	(Statistics.Sampling.uniform_f 1.0 ()) < prob;;


let getval low high = 
	Random.int (high-low+1) + low;;


let rec createbst n low high level p = 
	if (n <= 0) then Leaf
	else 
		(if ((tossCoin p) == false) then 
			(let x = (high+1) in 
				(let nodes = truncate(2.0**float_of_int(level)) in  
					Node (x, (createbst (n-nodes) low high (level+1) p),
						(createbst (n-nodes) low high (level+1) p)))
			)
		else
			(let x = (getval low high) in
				(let nodes = truncate(2.0**float_of_int(level)) in  
					Node (x, (createbst (n-nodes) low x (level+1) p),
						(createbst (n-nodes) x high (level+1) p))))
		)
;;


(*
let rec createbst n low high level p = 
	if (n <= 0) then Leaf
	else 
		(let x = (getval low high) in
			(let nodes = truncate(2.0**float_of_int(level)) in  
				Node (x, (createbst (n-nodes) low x (level+1) p),
					(createbst (n-nodes) x high (level+1) p))))
;;


*)


let rec print_tree tr = 
	match tr with 
	| Leaf -> print_string "Leaf"
	| Node (x,l,r) -> print_string "Node "; 
			print_string "(";
			print_string(string_of_int(x));
			print_string ", ";
			print_tree(l);	
			print_string ", ";
			print_tree(r);
			print_string "), ";;


print_tree(createbst 
		(int_of_string(Sys.argv.(1))) 
		0 200 0 0.9);;
Printf.printf "\n";
