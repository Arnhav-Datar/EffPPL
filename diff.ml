exception Unknown


module AD : sig
  
  type t
  val mk : float -> t
  val (+.) :  t -> t -> t
  val (-.) :  t -> t -> t
  val ( *. ) : t ->  t ->  t
  val ( /. ) : t ->  t ->  t
  val ( let* ) :  t -> (t -> t ) ->  t
  val grad  : ( unit ->  t) -> (t list * float list * float Primitive.t list)
  val samp : float Primitive.t ->  t
  val norm : t -> t -> t
  val beta : t -> t -> t
  val gamma : t -> t -> t
  val exp : t -> t
  val chi2 : t -> t
  val get : t -> float
  val get_der : ( unit ->  t) -> float list -> (float * t list)
  val get_val : ( unit ->  t) -> float list -> (float)
  val cond: bool -> t -> t -> t
  val hmc : ( unit ->  t)  -> int -> float -> int -> float list list
  val print_list : t list -> unit
  val print_normal_list : float list -> unit
  val print_sample_list : float list list -> unit
  val print_values : float list list -> ( unit ->  t) -> unit
  val get_samples: ( unit ->  t)  -> int -> float -> int -> float list
  val observe: t -> (float -> float) -> unit
  (* path_len -> step_size -> epochs -> samples *)

end = 

struct

	type t = { v : float;  mutable d : float ; m : float}

	let mk v = 
		{v; d = 0.0; m=1.0}
	
	let get t' = 
		match t' with 
			{v= v' ; d = _ ;  m = _} -> v'

	effect Add : t * t -> t
	effect Sub : t * t -> t
	effect Mult : t * t -> t
	effect Div : t * t ->  t
	effect Obs : t * (float -> float) ->  t
	effect Norm : t * t ->  t
	effect Beta : t * t ->  t
	effect Gamma : t * t ->  t
	effect Leet : t * (t -> t) -> t
	effect Samp : float Primitive.t ->  t
	
	
	let rec find_list v ls = 
		match ls with 
		| [] -> 
			raise Unknown
		| {v = v1; d= d1; m=_}::tl -> 
			if(v=v1) then d1 else find_list v tl

  	let modif_der ls vc dc = 
  		List.map 
  		(
  			fun {v=v'; d=d'; m=m'} -> 
	  			if(v'=vc) then  {v=v'; d=dc; m=m'}
	  			else {v=v'; d=d'; m=m'}
  		)  ls

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

  	let rec run_grad f ls sls pls=
		match f () with
		| r -> 
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r)
		
		| effect (Obs(_,_)) k ->  
			let x = {v = 0.0; d = 0.; m=1.} in
			(* print_endline "in obs";
			Printf.printf "Value %f\n" tu.v;
			Printf.printf "probab %f\n" (f tu.v); *)
			ignore (continue k x);
			(x)

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x)
		
		| effect (Leet(m,f')) _ ->
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1) = (run_grad (fun () -> f' m) ls sls pls) in
			(x1)

		| effect (Norm(mu,si)) k ->
			let m = get(mu) in
			let s = get(si) in
			let p = Primitive.(normal m s) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			ls := modif_der !ls m (mu.d +. x.d *. v2);
			let vf = (((v1 -. m) *. (v1 -. m)) -. (s *. s))/. (s *. s *. s) in
			ls := modif_der !ls s (si.d +. x.d *. vf);
			(x)
		
		| effect (Beta(a1,b1)) k ->
			let a = get(a1) in
			let b = get(b1) in
			let p = Primitive.(beta a b) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			ls := modif_der !ls a (a1.d +. x.d *. Float.log(v1));
			ls := modif_der !ls b (b1.d +. x.d *. Float.log(1.0 -. v1));
			(x)

		| effect (Gamma(k1,t1)) c ->
			let k = get(k1) in
			let t = get(t1) in
			let p = Primitive.(gamma k t) in 
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue c x);
			let dn = find_list v1 !ls  in 
			ls := modif_der !ls v1 (dn *. v2);
			let td = (v1 /. (t *. t)) -. (k /. t) in 
			let kd = (Float.log (v1 /. t)) -. (Owl_maths.psi k)  in 
			ls := modif_der !ls k (k1.d +. x.d *. kd);
			ls := modif_der !ls t (t1.d +. x.d *. td);
			(x)

  	let grad f =
  		let rls = ref [] in 
  		let sls = ref [] in 
  		let pls = ref [] in 
		let _ = run_grad f rls sls pls in 
		(!rls,!sls,!pls)

	let rec get_val' f ls' = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			r

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			(continue k x);
			
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			(continue k x);
			
		| effect (Obs(_,_)) k ->
			let x = {v = 0.0; d = 0.;  m=1.} in
			(continue k x);


		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			(continue k x);
			
		
		| effect (Leet(m,f')) _ ->
			(* let x = {v = m.v; d = 0.0; m=m.m} in  *)
			let x1 = (get_val' (fun () -> f' m) ls')  in
			x1


		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
				
		end

		| effect (Gamma (k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				let p = Primitive.(gamma k t) in  
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue c x);
				
		end

	let get_val f sls =
		let rsls = ref sls in 
		let x1 = ( get_val' f rsls) in
		(x1.v)

	let rec get_der' f ls' ls = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r,ls)
		
		| effect(Obs(_,_)) k ->
			let x = {v = 0.0; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			(rv, ls)

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Leet(m,f')) _ ->
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1,_) = (get_der' (fun () -> f' m) ls' ls)  in
			(x1,ls)


		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				(* Printf.printf "Norm %f\n" v1; *)
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
				
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				mu.d <- mu.d +. x.d *. v2;
				ls := modif_der !ls m (mu.d);
				let vf = (((v1 -. m) *. (v1 -. m)) -. (s *. s))/. (s *. s *. s) in
				ls := modif_der !ls s (si.d +. x.d *. vf);
				(rv, ls)
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				(* Printf.printf "Beta %f\n" v1; *)
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
					
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				ls := modif_der !ls a (a1.d +. x.d *. Float.log(v1));
				ls := modif_der !ls b (b1.d +. x.d *. Float.log(1.0 -. v1));
				(rv, ls)
		end

		| effect (Gamma(k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				(* Printf.printf "Gamma %f\n" v1; *)
				let p = Primitive.(gamma k t) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue c x) in
				let dn = find_list v1 !ls  in 
				ls := modif_der !ls v1 (dn *. v2);
				let td = (v1 /. (t *. t)) -. (k /. t) in 
				let kd = (Float.log (v1 /. t)) -. (Owl_maths.psi k)  in 
				ls := modif_der !ls k (k1.d +. x.d *. kd);
				ls := modif_der !ls t (t1.d +. x.d *. td);
				(rv, ls)
		end

	let get_der f sls =
		let rls = ref [] in
		let rsls = ref sls in 
		let (x1, _) = ( get_der' f rsls rls ) in
		(x1.v, !rls)

  	let samp p = 
  		perform (Samp(p))

  	let norm mu si = 
  		perform (Norm(mu,si))

	let beta a b = 
		perform (Beta(a,b))
 
 	let observe t fu =
 		ignore (perform (Obs(t,fu)))

	let gamma k t = 
		perform (Gamma(k,t))

 	let norm_list n = 
 		List.init n (fun _ -> Primitive.sample (Primitive.normal 0. 1.)) 

 	let listadd l1 l2 mul= 
 		List.map2 (fun x y -> x +. (y *. mul) ) l1 l2 

 	

 	let rec subs l1 l2 l3= 
 		match l1 with 
 		| [] -> l3
 		| hd::tl -> 
 			match l2 with 
 			| [] -> l3
 			| h::t -> 
 				if(h=hd.v) then
 					subs tl t (hd.d::l3)
 				else
 					subs tl l2 l3

 	let rec get_obs_log' f ls' vl = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			r

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			(continue k x);
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			(continue k x);
	
			
		| effect (Div(a,b)) k -> begin
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			(continue k x);
		end
			
		| effect (Obs(tu,f)) k -> begin
			let x = {v = 0.0; d = 0.;  m=1.} in
			(* Printf.printf "%f %f \n" tu.v (f tu.v) ;  *)
			ignore (vl := !vl -. ((f tu.v)));
			(continue k x);
		end		
		
		| effect (Leet(m,f')) _ ->
			let x1 = (get_obs_log' (fun () -> f' m) ls' vl)  in
			x1

		| effect (Norm(mu,si)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let m = get(mu) in
				let s = get(si) in
				let p = Primitive.(normal m s) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
					
		end

		| effect (Beta(a1,b1)) k -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
				let p = Primitive.(beta a b) in 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue k x);
				
		end

		| effect (Gamma (k1,t1)) c -> begin
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let k = get(k1) in
				let t = get(t1) in
				let p = Primitive.(gamma k t) in  
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				(continue c x);
				
		end


 	let get_obs_log f sls =
		let rsls = ref sls in 
		let vl = ref 0.0 in 
		let _ = ( get_obs_log' f rsls vl) in
		(* Printf.printf "%f\n" !vl; *)
		(!vl)


 	let nlp q pls f =
 		assert(List.length q = List.length pls);
 		let a1 = List.fold_left2 ( fun acc y p -> acc -. (Primitive.logpdf p y)) 0. q pls in 
 		let a2 = get_obs_log f q in 
 		(* Printf.printf "%f %f\n" a1 a2;  *)
 		a1 +. a2 

 	let nlp1 p = 
 		List.fold_left (fun acc y -> acc -. (Primitive.logpdf (Primitive.normal 0. 1.) y)) 0. p

 	let get_hmc_grad f q = 
 		let (_, dv) = get_der f q in
		let dv = List.rev ( subs dv q [] ) in 
		List.map (fun f -> f *. (-. 1.0)) (dv) 

	let rec leapfrog (li:int ) (stp:float) (p1:float list) (q1:float list) f =
 		let dVdQ = get_hmc_grad f q1 in 
 		(* Printf.printf "Leapfrog iteration %d\n" li; *)
 		if(li = 0) then
 			(p1, q1)
 		else 
 			let p1 = listadd p1 dVdQ (stp/.2.0) in 
 			let q1 = listadd q1 p1 1.0 in 
 			let dVdQ1 = get_hmc_grad f q1 in 
 			(* Printf.printf "Leapfrog iteration %d\n" li; *)
 			let p1 = listadd p1 dVdQ1 (stp/.2.0) in 
 			leapfrog (li -1) stp p1 q1 f

	let rec hmc' (f: ( unit ->  t) ) (li:int) (stp:float) (ep:int) (samp_list: float list list) pls =
		if(ep=0) then
			samp_list
		else
			let q0 = List.nth samp_list (List.length samp_list - 1) in
			let q1 = q0 in
			(* let _ =
				if(ep mod 25 = 0) then
					Printf.printf "Left with %d epochs\n" (ep)
			in *)

			let p0 = norm_list (List.length q1) in
			let p1 = p0 in

			let (p1, q1) = leapfrog li stp p1 q1 f in
			let p1 = List.map (fun f -> f *. (-. 1.0)) p1 in 

			(* print_normal_list q0; *)
			(* print_normal_list q1; *)
			(* print_normal_list dvdq; *)
			(* print_endline "==============="; *)

			let p0p = nlp1 p0 in
			let p1p = nlp1 p1 in
			let q0p = nlp q0 pls f in
			let q1p = nlp q1 pls f in

			let tgt = q0p -. q1p in
			let adj = p1p -. p0p in 
			let acc = tgt +. adj in

			let x' = Primitive.sample (Primitive.continuous_uniform 0. 1.) in
			let x = Float.log x' in
			
			if (x < acc) then begin
				(* print_normal_list q1; *)
				hmc' f li stp (ep-1) (samp_list@[q1]) pls
			end
			else
				hmc' f li stp (ep-1) (samp_list@[q0]) pls

	let hmc (f: ( unit ->  t) ) (li:int)  (stp:float) (ep:int) : float list list =
		let (_, smp, pls) = grad f in 
		hmc' f li stp ep [smp] pls

	let print_sample_list ls =
		List.iter (fun ll -> print_normal_list ll) ls

	let print_values ls f =
		List.iter (fun ll-> 
			let x = get_val f ll in 
			Printf.printf "%f\n" x;
		) ls

	let get_samples (f: ( unit ->  t) ) (li:int ) (stp:float) (ep:int) =
		List.map (fun ll->  get_val f ll ) (hmc f li stp (ep-1))

	let (+.) a b = 
		perform (Add(a,b))
  	
  	let (-.) a b = 
  		perform (Sub(a,b))

  	let ( *. ) a b = 
  		perform (Mult(a,b))
  	
  	let ( /. ) a b = 
  		perform (Div(a,b))

  	let exp l =
		perform (Gamma(mk 1.0, (mk 1.0) /. l )) 

	let chi2 k =
		perform (Gamma( k /. (mk 2.0) , mk 2.0) )

  	let (let*) m f = 
  		perform (Leet(m,f))

  	let cond b y n =

  		if b then y else n
end;;


(*Test 1*)

print_endline "+++++++++++++++++++++++++++++++++++++++";
print_endline "=======================================";
print_endline "Checking Helper functions";
print_endline "=======================================";
print_endline "+++++++++++++++++++++++++++++++++++++++";

open AD

let f1 () = 
	let* x1 = norm (mk 2.) (mk 1.) in
	let* x2 = norm (mk 2.) (mk 1.) in
	let* x3 = norm (mk 2.) (mk 1.) in
	let* x4 = 	x1 +. x2 +. x3 in 
	(observe x4 (Primitive.pdf (Primitive.(normal 6.0 3.0 ))));
	let* x5 = 	x1 *. x2 *. x3 in 
	(observe x5 (Primitive.pdf (Primitive.(normal 8.0 3.0 ))));
	x4 +. x5
;;


let (ls,smp,_) =  grad f1  in
Printf.printf "All values and derivatives\n";
print_list ls;
print_endline "";

Printf.printf "All samplings\n";
print_normal_list smp;
print_endline "";

Printf.printf "Getting it back functions \n";
let (x1,ls1) =  get_der f1 smp in
print_list ls1;
Printf.printf "%f \n\n" x1;

let x1 = get_val f1 smp in 
Printf.printf "%f \n\n" x1; 




(* print_endline "+++++++++++++++++++++++++++++++++++++++";
print_endline "=======================================";
print_endline "Checking A simple HMC for my DL exam";
print_endline "=======================================";
print_endline "+++++++++++++++++++++++++++++++++++++++";


let f2 () =
	let* x1 = norm (mk 45.11363636) (mk 14.54225817) in
	let* x2 = norm (mk 37.8358209) (mk 15.56563281) in
	let* x3 = x1 /. (mk 70.0) in
	let* x4 = x2 /. (mk 70.0) in
	let* x5 = if (get(x3) > get(x4)) then x3 else x4 in
	let* x6 = if (get(x3) > get(x4)) then x4 else x3 in
	let* x7 = (mk 5. /. mk 12.) *. x6 +. (mk 7. /. mk 12.) *. x5 in
	let* x8 = x7 *. (mk 65.) in 
	(observe x8 (Primitive.logpdf (Primitive.(normal 50.0 10.0 ))));
	x8

in 


let epochs = 1000 in
let fils = Array.of_list (AD.get_samples f2 4 0.25 epochs) in

let mn = (Owl_stats.mean fils) in 
let st = (Owl_stats.std fils) in 
Printf.printf "Mean = %f\n" mn;
Printf.printf "Std. Dev. = %f\n" st; *)


(* Printf.printf "Normal 0 1 logpdf at 1 = %f\n" (Primitive.logpdf (Primitive.normal 0. 1. ) 100.)  *)


print_endline "+++++++++++++++++++++++++++++++++++++++";
print_endline "=======================================";
print_endline "Linear Regression Data";
print_endline "=======================================";
print_endline "+++++++++++++++++++++++++++++++++++++++"; 


let nrm () =
	let* x = norm (mk 0.) (mk 9.) in 
	x
in 

let obs_points = 200 in 
let lx = List.init obs_points (fun x-> Float.of_int x) in 
let er = AD.get_samples nrm 4 0.25 obs_points in
let ly' = List.map (fun x -> Float.add (Float.mul 2.0 x) 20.0) lx in
let ly = List.map2 (fun x y -> Float.add x y) ly' er in
let ax = Array.of_list lx in 
let ay = Array.of_list ly in 
(* print_normal_list lx; *)
(* print_normal_list ly; *)
 
let lin () =
	let* m = norm (mk 1.) (mk 3.) in 
	let* c = norm (mk 10.) (mk 10.) in 
	let* s1 = norm (mk 0.) (mk 3.) in 
	let* s = s1 *. s1 in 
	ignore (
	for i = 0 to (obs_points-1) do 
		observe ((mk ay.(i)) -. m*.(mk ax.(i)) -. c) (Primitive.logpdf Primitive.(normal 0. (get s)))
	done );
	m
in 

let epochs = 10000 in
let fils = Array.of_list (AD.get_samples lin 2 0.005 epochs) in

let mn = (Owl_stats.mean fils) in 
let md = (Owl_stats.median fils) in 
Printf.printf "Mean slope = %f\n" mn;
Printf.printf "Median slope = %f\n" md;
(* for i = 0 to epochs-1 do
	Printf.printf "%f \n" fils.(i);
done ; *)


let lin () =
	let* m = norm (mk 1.) (mk 3.) in 
	let* c = norm (mk 10.) (mk 10.) in 
	let* s1 = norm (mk 0.) (mk 3.) in 
	let* s = s1 *. s1 in 
	ignore (
	for i = 0 to (obs_points-1) do 
		observe ((mk ay.(i)) -. m*.(mk ax.(i)) -. c) (Primitive.logpdf Primitive.(normal 0. (get s)))
	done );
	c
in 

let epochs = 10000 in
let fils = Array.of_list (AD.get_samples lin 2 0.005 epochs) in

let mn = (Owl_stats.mean fils) in 
let md = (Owl_stats.median fils) in 
(* let st = (Owl_stats.std fils) in  *)
Printf.printf "Mean constant = %f\n" mn;
Printf.printf "Median constant = %f\n" md;
(* Printf.printf "Std. Dev. = %f\n=================\n" st;  *)


List.iter2 (fun x y -> Printf.printf "(%f, %f) \n" x y) lx ly