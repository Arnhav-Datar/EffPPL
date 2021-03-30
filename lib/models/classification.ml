open Effppl.Infer;;
(*
	Defining some of the constants
*)
let epochs = 10000 in
let px = 5 in 
let py = 5 in 
let obs_points = (1+px)*(1+py) in 

(*
	Does a linear classification. 
*)

let lin obs_points ax1 ax2 ay () =
	let* m1 = normal 0. 2. in 
	let* m2 = normal 0. 2. in 
	for i = 0 to (obs_points-1) do 
		observe (m1*.(mk ax1.(i)) +. m2*.(mk ax2.(i)) +. (mk 1.0)) (fun x -> if( (Float.mul x ay.(i))>0.) then 1. else -1.)
	done;
	m1
in
(*
	generating the data
*)
let lx = List.init (1+px) (fun x -> Float.mul (Float.of_int x) 0.2) in 
let ly = List.init (1+py) (fun x -> Float.mul (Float.of_int x) 0.2) in 
let al = List.map (fun x -> List.map (fun y-> (x,y)) ly) lx in
let alc = List.concat al in
let ax1 = Array.of_list ( List.map (fun (x,_) -> x) alc) in 
let ax2 = Array.of_list ( List.map (fun (_,y) -> y) alc) in 
let lbl = Array.of_list (List.map (fun (x,y) -> if ((Float.add x y) < 1.1) then 1. else -1. ) alc) in 

(*
	Printing the results of sample mean of the x and y slopes.
*)
let fils = (hmc (lin obs_points ax1 ax2 lbl) 2 0.05 epochs) in
let ff = Array.of_list ( List.map (fun ls -> List.hd ls) fils) in 
let ss = Array.of_list ( List.map (fun ls -> List.nth ls 1) fils) in 
Printf.printf "Mean slope 1 = %f \n" (Owl_stats.mean ff);
Printf.printf "Mean slope 2 = %f \n" (Owl_stats.mean ss);