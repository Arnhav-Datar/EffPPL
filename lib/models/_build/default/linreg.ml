open Effppl.Diff.HMC

let nrm () =
	let* x = normal 0. 0.5 in 
	x
;; 

let lin obs_points ax ay () =
	let* m = normal 1. 3. in 
	let* c = normal 7. 10. in 
	let* s1 = normal 0. 3. in 
	let* s = s1 *. s1 in 
	for i = 0 to (obs_points-1) do 
		observe ((mk ay.(i)) -. m*.(mk ax.(i)) -. c) (Effppl.Primitive.logpdf Effppl.Primitive.(normal 0. (get s)))
	done ;
	m
;; 

let obs_points = 200 in 
let epochs = 10000 in

let lx = List.init obs_points (fun x-> Float.of_int x) in 
let er = get_samples nrm 4 0.05 obs_points in
let ly' = List.map (fun x -> Float.add (Float.mul 2.0 x) 10.0) lx in
let ly = List.map2 (fun x y -> Float.add x y) ly' er in
let ax = Array.of_list lx in 
let ay = Array.of_list ly in 


let fils = (hmc (lin obs_points ax ay) 2 0.005 epochs) in
let mcl = List.map (fun ls -> (List.nth ls 0, List.nth ls 1)) fils in 
let sm =  List.map (fun (ax, _) -> ax) mcl in  
let sma =  Array.of_list sm in  
let sc =  List.map (fun (_, ay) -> ay) mcl in  
let sca =  Array.of_list sc in  

let mns = Owl_stats.mean sma in 
let mnc = Owl_stats.mean sca in

Printf.printf "%f %f \n" mns mnc; 