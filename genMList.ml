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

(* Need to describe dist *)


(* Input generator functions *)
let rec list_inputgen lst f dist size =
  if (size == 0) then lst
  else
    (match dist with
     | [] -> []
     | h::t -> if h then
                  (match lst with
                   | [] -> [(new_elem_acc f)]
                   | l' -> (list_inputgen ((new_elem_acc f) :: l') 
				f t (size-1)))
               else
                 (match lst with
                  | [] -> [(new_elem_vio f)]
                  | l' -> (list_inputgen ((new_elem_vio f) :: l') 
				f t (size-1))))


(*let rec list_inputgen lst f dist size =
  if (size == 0) then lst
  else
    if dist then
      (match lst with
       | [] -> [(new_elem_acc f)]
       | l' -> (list_inputgen ((new_elem_acc f) :: l') 
				f dist (size-1)))
    else
      (match lst with
       | [] -> [(new_elem_vio f)]
       | l' -> (list_inputgen ((new_elem_vio f) :: l') 
				f dist (size-1)))
*)

(* Initial function -- gets the samples from anglican *) 

let create_inputs prop likelihood elems dist_samples =
    (*list_inputgen [] prop dist_samples elems*)
    list_inputgen [] even_number [true;false] 2
  
  
  
