open Z3
open Z3.Symbol
open Z3.Sort
open Z3.Expr
open Z3.Boolean
open Z3.Goal
open Z3.Probe
open Z3.Solver
open Z3.Model
open Z3.FuncDecl

let boolTest ( ctx  : context ) =
	Printf.printf "Boolean Check\n";
	let x = (mk_string ctx "x") in
	let y = (mk_string ctx "y") in
	let sx = (Boolean.mk_sort ctx) in
	let sy = (Boolean.mk_sort ctx) in
	let fx = (FuncDecl.mk_func_decl ctx x [] sx) in 
	let fy = (FuncDecl.mk_func_decl ctx y [] sy) in
 	let notx = (Boolean.mk_not ctx (FuncDecl.apply fx [])) in
	let noty = (Boolean.mk_not ctx (FuncDecl.apply fy [])) in
	let orxy = (Boolean.mk_or ctx [notx;noty]) in
	let anxy = (Boolean.mk_and ctx [(FuncDecl.apply fx []);(FuncDecl.apply fy [])]) in
	let ntan = (Boolean.mk_not ctx anxy) in
	let eqst = (Boolean.mk_eq ctx ntan orxy) in 
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
				
