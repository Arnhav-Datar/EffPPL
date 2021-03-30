open Effppl.Infer
open Effppl.Print 

(*
	moving-average model for time series analysis
	A moving average model uses previous errors as predictors for future outcomes. For a moving average model of order Q,  
	MA(Q), there is an overall mean parameter μ and regression coefficients θq for previous error terms. With ϵt
  	being the noise at time t, the model for outcome yt
  	 yt = μ + th1 ϵt-1 + ... + thQ ϵt-Q + ϵt

  	with the noise term ϵt  for outcome  yt modeled as normal noise

*)
let ma obs_points ay () =
	
	let* mu = cauchy 0. 3. in 
	let* th1 = cauchy 0. 3. in 
	let* th2 = cauchy 0. 3. in 
	
	let* ep1 = (mk ay.(0)) -. mu in 
	let* ep2 = (mk ay.(1)) -. mu -. (th1 *. ep1) in 
	
	for i = 2 to (obs_points-1) do 

		ignore (
			let* t1 = ep1 in 
			let* t2 = ep2 in 
			let* ep1 = t2 in 
			let* ep2 = (mk ay.(i)) -. (mu) -. (th1*.ep1) -. (th2*.t1)  in 
			observe (ep2) (Effppl.Primitive.logpdf Effppl.Primitive.(normal 0. 2.));
			(mk 1.)
		)
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

let fils = (hmc (ma obs ax) 2 0.0001 epochs) in

let mcl = List.map (fun ls -> (List.nth ls 0, List.nth ls 1)) fils in 
let sm =  List.map (fun (ax, _) -> ax) mcl in  
let sc =  List.map (fun (_, ay) -> ay) mcl in  

print_statistics sm;
print_statistics sc;

(* let mns = Owl_stats.mean sma in  *)
(* let mnc = Owl_stats.mean sca in *)
(* print_normal_list sm; *)
(* Printf.printf "%f %f \n" mns mnc;  *)

(*
	The beta converges fast, but the alpha requires a lot of data, because of its lower relevance.
*)