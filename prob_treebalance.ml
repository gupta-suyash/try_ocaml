open Oml

type color = Red 
| Black;;

type 'a mytree = 
| Leaf 
| Node of color * 'a * 'a mytree * 'a mytree;;


let tossCoin prob = 
	(Statistics.Sampling.uniform_f 1.0 ()) < prob;;


let getval low high = 
	Random.int (high-low+1) + low;;


let getcolor h color = 
	if (color == Red) then (h-1, Black) 
	else 
		if ((Random.int 2) == 0) then (h, Red)
		else (h-1, Black);;


let rec createbaltree h color low high = 
	if (h == 0) then Leaf
	else 
		(let x = (getval low high) in 
			(let child = (getcolor h color) in 
				Node (color, x, (createbaltree (fst child) (snd child) low high),
					(createbaltree (fst child) (snd child) low high))))
;;


let print_color c = 
	match c with 
	| Red -> print_string "Red "
	| Black -> print_string "Black ";;

let rec print_tree tr = 
	match tr with 
	| Leaf -> print_string "Leaf"
	| Node (c,x,l,r) -> print_string "Node "; 
			print_string "(";
			print_color c;
			print_string(string_of_int(x));
			print_string ", ";
			print_tree(l);	
			print_string ", ";
			print_tree(r);
			print_string "), ";;


print_tree(createbaltree 
		(int_of_string(Sys.argv.(1))) 
		Red 0 200);;
Printf.printf "\n";
