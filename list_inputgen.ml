type 'a mylist = 
| Nil
| Cons of 'a * 'a mylist;;


 let even_number a =
    if (a mod 2 == 0) then true
    else false;;


(* Functional-property functions *)
let rec new_elem_acc f =
  let x = (Random.int 100) in
    if (f x) then x
    else new_elem_acc f

let rec new_elem_vio f =
 let x = (Random.int 100) in 
   if (not (f x)) then x
   else new_elem_vio f


(* Input generator functions *)
let rec list_inputgen size samples f =
  if (size == 0) then [] 
  else
    (match samples with
	| [] -> [] 
	| h::t -> if h 	then
			((new_elem_acc f) :: (list_inputgen (size-1) t f))
			else 
			 ((new_elem_vio f) :: (list_inputgen (size-1) t f)));;


(* The executable line -- passes 3 arguments to the list_inputgen function *)
(List.iter (Printf.printf "%d ") (list_inputgen 5 [true;false;false;true;false] even_number));;


