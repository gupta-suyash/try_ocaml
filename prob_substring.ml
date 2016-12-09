open Oml

type 'a mylist = 
| Nil 
| Cons of 'a * 'a mylist;;


let tossCoin prob = 
	(Statistics.Sampling.uniform_f 1.0 ()) < prob;;


let getval low high = 
	Random.int (high-low+1) + low;;


let rec getElem str pos = 
	match str with 
	| Nil -> 0
	| Cons (h,t) -> 
		(if (pos == 0) then h 
		else (getElem t (pos-1)))
;;


let rec createString n low high = 
	if (n <= 0) then Nil
	else 
		(let x = (getval low high) in 
			Cons (x, (createString (n-1) low high)) 
		)	
;;



let rec createSubString n str pos p = 
	if (n <= 0) then Nil
	else 
		(if ((tossCoin p) == false) then 
			(let x = (getElem str (getval 0 10)) in 
				Cons (x, (createSubString (n-1) str (pos+1) p)))
		else
			(let x = (getElem str pos) in 
				Cons (x, (createSubString (n-1) str (pos+1) p)))	
		)	
;;


(*
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
*)




let rec print_list str = 
	match str with 
	| Nil -> print_string "Nil"
	| Cons (h,t) -> print_string "Cons "; 
			print_string "(";
			print_string(string_of_int(h));
			print_string ", ";
			print_list(t);	
			print_string ")";;


let s1 = createString (int_of_string(Sys.argv.(1))) 0 200;;
let s2 = createSubString 5 s1 3 0.5;;

print_list(s1);;
Printf.printf "\n";
print_list(s2);;
Printf.printf "\n";



