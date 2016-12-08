let rec factorial x = 
	match x with
	| 0 -> 1
	| n -> n * (factorial (n-1))
in
print_int (factorial 5);;
