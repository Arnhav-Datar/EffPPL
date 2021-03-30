open Helpers.Basics

module type S' = sig
  
	type t
	
	val print_list : t list -> unit
	
	val print_normal_list : float list -> unit
	
	val print_sample_list : float list list -> unit
	
	val print_statistics : float list -> unit

end 

module Print : S' =

struct

	type t = { v : float;  mutable d : float ; m : float}

	let print_list ls = 
		List.iter 
  		(
  			fun {v=v'; d=d'; m=_} -> 
	  			Caml.Printf.printf "%f %f \n" v' (d');
  		) ls

  	let print_normal_list ls = 
		List.iter 
  		(
  			fun v -> 
	  			Caml.Printf.printf "%f \n" v;
  		) ls;
  		print_endline ""

	let print_sample_list ls =
		List.iter 
		(
			fun ll -> 
				print_normal_list ll
		) ls

	let print_statistics ls =
		let mean = get_mean ls in
		Printf.printf "Mean = %f \n" mean; 
		let md = get_median ls in 
		Printf.printf "Median = %f \n" md; 
		let (min,max) = get_min_max ls in 
		Printf.printf "Minimum = %f \n" min; 
		Printf.printf "Maximum = %f \n" max; 
		let std = get_std ls in 
		let var = get_var ls in
		Printf.printf "Std. Dev. = %f \n" std; 
		Printf.printf "Variance = %f \n" var; 

end;;
