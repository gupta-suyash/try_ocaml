open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Goal
open Z3.Probe
open Z3.Solver
open Z3.Model
open Z3.Arithmetic
open Z3.Arithmetic.Integer

let rec addToGoal ctx decl =
	match decl with
	| [] -> []
	| h::t -> (let cs = (Integer.mk_sort ctx) in 
		  let ex = (Expr.mk_const ctx (Symbol.mk_int ctx 0) cs) in 
		  Boolean.mk_eq ctx h ex)	 
			:: addToGoal ctx t

let boolTest ctx gset =
	Printf.printf "Boolean Check\n";
	let g = (mk_goal ctx true false false) in
  	(Goal.add g gset);
  	Printf.printf "%s\n" ("Goal: " ^ (Goal.to_string g)) ;
  	(
    	let solver = (mk_solver ctx None) in
    	(List.iter (fun a -> (Solver.add solver [ a ])) (get_formulas g)) ;
    	if (check solver []) != SATISFIABLE then
      		Printf.printf "Failure\n"
    	else
      	(
		Printf.printf "Test passed.\n";
		let gtmod = Solver.get_model solver in 
		match gtmod with
		| None -> Printf.printf "Exception occurred"
		| Some (m) -> Printf.printf "%s\n" (Model.to_string m)
	))

let rec addVarsToCtx ctx count = 
	match count with 
	| 0 -> []
	| v -> (let vr = (Integer.mk_sort ctx) in 
		  (Expr.mk_const ctx (Symbol.mk_string ctx 
		  ("a" ^ (string_of_int(count)))) vr)) :: 
			(addVarsToCtx ctx (count-1))


let _ = 
  try (
    if not (Log.open_ "z3.log") then
    	Printf.printf "Failure with log."
    else
	(
		Printf.printf "Running Z3 version %s\n" Version.to_string ;
		Printf.printf "Z3 full version string: %s\n" Version.full_version ;
		let cfg = [("model", "true"); ("proof", "false")] in
		let ctx = (mk_context cfg) in 
		let decl = addVarsToCtx ctx 2 in
		let gset = addToGoal ctx decl in 
		boolTest ctx gset
	);
		exit 0
	) with Error(msg) -> (
		Printf.printf "Z3 EXCEPTION: %s\n" msg ;
		exit 1
	)
				
