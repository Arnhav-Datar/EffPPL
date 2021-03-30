open Effppl.Infer

let f1 () = 
	let* x1 = cauchy 0. 1. in
	(* let* x2 = cauchy 1. 4. in *)
	x1 
;;
(*
	if X ~ N(m1, s1) and Y ~ N(m2, s2) 
	we know that the resulting distribution is also normal
	furthermore the resulting distribution will have
	mu = m1 + m2
	and 
	s^2 = s1^2 + s2^2
*)

let x = get_samples f1 3 0.02 10000 in
let ax = Array.of_list x in
let am = (Owl_stats.mean ax) in
let ast = (Owl_stats.median ax) in
Printf.printf "Mean = %f \n" am;
Printf.printf "Median = %f \n" ast;
(*
	We therefore expect for large enough samples 
	mean = 2
	std.dev. = 5
	Since this is sampling its bound to be slightly inaccurate.
*)