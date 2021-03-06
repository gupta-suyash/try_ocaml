open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Goal
open Z3.Probe
open Z3.Solver
open Z3.Model

let boolTest ( ctx  : context ) =
	Printf.printf "Boolean Check\n";
	let a = (Boolean.mk_sort ctx) in 
	let b = (Boolean.mk_sort ctx) in 
	let ac = (Expr.mk_const ctx (Symbol.mk_string ctx "a") a) in
 	let bc = (Expr.mk_const ctx (Symbol.mk_string ctx "b") b) in
 	let nota = (Boolean.mk_not ctx ac) in
	let orab = (Boolean.mk_or ctx [nota;bc]) in
	let eqst = (Boolean.mk_eq ctx ac orab) in 
	let g = (mk_goal ctx true false false) in
  (Goal.add g [ eqst ]) ;
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
			)
  )


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
				boolTest ctx
			);
		exit 0
	) with Error(msg) -> (
		Printf.printf "Z3 EXCEPTION: %s\n" msg ;
		exit 1
	)
				
