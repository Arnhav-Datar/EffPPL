open Effppl.Infer
open Effppl.Print 

(*
	autoregressor model for time series analysis
	A first-order autoregressive model (AR(1)) with normal noise 
	takes each point y(n) in a sequence to be generated according to
		y(n) ~ Normal ( alpha + beta.y(n-1), sigma)
*)
let autoreg obs_points ay () =
	let* alp = normal 0. 3. in 
	let* bet = normal 1. 1. in 
	for i = 0 to (obs_points-2) do 
		observe ((mk ay.(i+1)) -. alp -. bet*.(mk ay.(i))) (Effppl.Primitive.logpdf Effppl.Primitive.(normal 0. 2.))
	done ;
	(mk 1.)
;; 

(*
	We generate the data with beta = 1.1
	and alpha = 2
*)
let epochs = 10000 in
let ls = [1.0; 1.845946117493874; 2.4031367889696877; 5.213008219953643; 6.569541733525667; 9.96215742323023; 14.087626273739925; 16.949029368670313; 20.581132240245033; 22.630352573084505] in
let ax = Array.of_list ls in 
let obs = Array.length ax in 
let fils = (hmc (autoreg obs ax) 2 0.0001 epochs) in

let mcl = List.map (fun ls -> (List.nth ls 0, List.nth ls 1)) fils in 
let sm =  List.map (fun (ax, _) -> ax) mcl in  
let sma =  Array.of_list sm in  
let sc =  List.map (fun (_, ay) -> ay) mcl in  
let sca =  Array.of_list sc in  

let mns = Owl_stats.mean sma in 
let mnc = Owl_stats.mean sca in
print_normal_list sm;
Printf.printf "%f %f \n" mns mnc; 

(*
	The beta converges fast, but the alpha requires a lot of data, because of its lower relevance.
*)