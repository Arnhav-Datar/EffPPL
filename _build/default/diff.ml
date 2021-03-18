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
  val samp :float Primitive.t ->  t
  val norm : t -> t -> t
  val beta : t -> t -> t
  val get : t -> float
  val get_der : ( unit ->  t) -> float list -> (float * t list)
  val get_val : ( unit ->  t) -> float list -> (float)
  val cond: bool -> t -> t -> t
  val hmc : ( unit ->  t)  -> float -> float -> int -> float list list
  val print_list : t list -> unit
  val print_normal_list : float list -> unit
  val print_sample_list : float list list -> unit
  val print_values : float list list -> ( unit ->  t) -> unit
  val get_list: ( unit ->  t)  -> float -> float -> int -> float list
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
	effect Norm : t * t ->  t
	effect Beta : t * t ->  t
	effect Leet : t * (t -> t) -> t
	effect Samp : float Primitive.t ->  t
	
	
	let rec find_list v ls = 
		match ls with 
		| [] -> 
			raise Unknown
		| {v = v1; d= d1; m=_}::tl -> 
			if(v=v1) then d1 else find_list v tl



  	let modif_der ls vc dc = 
  		(* Printf.printf "Modifying %f to %f\n" vc dc;  *)
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
	  			Caml.Printf.printf "%f " v;
  		) ls;
  		print_endline ""

  	let rec run_grad f ls sls pls=
		match f () with
		| r -> 
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r)
		
		| effect (Add(a,b)) k ->
			(* print_endline "in add"; *)
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(* print_endline "in add"; *)
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
			(* print_endline "in let"; *)
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1) = (run_grad (fun () -> f' m) ls sls pls) in
			(x1)

		(* | effect (Samp(p)) k ->
			(* print_endline "in samp"; *)
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			pls := !pls@[p];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			ls := modif_der !ls v1 (x.d *. v2);
			(x) *)

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
		
		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			let (rv,_) =  (continue k x) in
			(* ignore (continue k x); *)
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(* (x, ls) *)
			(rv, ls)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			(* ignore (continue k x); *)
			let (rv,_) =  (continue k x) in
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(* (x, ls) *)
			(rv, ls)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			(* ignore (continue k x); *)
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(rv, ls)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			let (rv,_) =  (continue k x) in
			(* ignore (continue k x); *)
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

		| effect (Beta(a1,b1)) k -> 
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl ->
				let a = get(a1) in
				let b = get(b1) in
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
 
 	let norm_list n = 
 		List.init n (fun _ -> Primitive.sample (Primitive.normal 0. 1.)) 

 	let listadd l1 l2 mul= 
 		List.map2 (fun x y -> x +. (y *. mul) ) l1 l2 

 	let rec leapfrog (pl:float) (stp:float) (p1:float list) (q1:float list) (dVdQ:float list) =
 		if(stp >= pl) then
 			(p1, q1)
 		else 
 			let p1 = listadd p1 dVdQ (stp/.2.0) in 
 			let q1 = listadd q1 p1 1.0 in 
 			let p1 = listadd p1 dVdQ (stp/.2.0) in 
 			leapfrog (pl -. stp) stp p1 q1 dVdQ 

 	let rec subs l1 l2 l3= 
 		match l1 with 
 		| [] -> l3
 		| hd::tl -> begin
 			match l2 with 
 			| [] -> l3
 			| h::t -> 
 				if(h=hd.v) then
 					subs tl t (hd.d::l3)
 				else
 					subs tl l2 l3
 		end

 	let nlp q pls =
 		assert(List.length q = List.length pls);
 		List.fold_left2 (
 			fun acc y p -> acc -. (Primitive.logpdf p y)) 0. q pls

 	let nlp1 p = 
 		List.fold_left (fun acc y -> acc -. (Primitive.logpdf (Primitive.normal 0. 1.) y)) 0. p

 	(* let nlp2  *)

	let rec hmc' (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) (samp_list: float list list) pls =
		if(ep=0) then
			samp_list
		else
			let q0 = List.nth samp_list (List.length samp_list - 1) in
			let q1 = q0 in
			(* let _ =
				if(ep mod 250 = 0) then
					Printf.printf "Left with %d epochs\n" (ep)
			in *)

			let p0 = norm_list (List.length q1) in
			let p1 = p0 in

			let (_, dv) = get_der f q1 in
			let dv = List.rev ( subs dv q1 [] ) in 
			let dvdq = List.map (fun f -> f *. (-. 1.0)) (dv) in

			let (p1, q1) = leapfrog pl stp p1 q1 dvdq in
			let p1 = List.map (fun f -> f *. (-. 1.0)) p1 in 

			let p0p = nlp1 p0 in
			let p1p = nlp1 p1 in
			let q0p = nlp q0 pls in
			let q1p = nlp q1 pls in

			let tgt = q0p -. q1p in
			let adj = p1p -. p0p in 
			let acc = tgt +. adj in

			let x' = Primitive.sample (Primitive.continuous_uniform 0. 1.) in
			let x = Float.log x' in
			
			if (x < acc) then
				hmc' f pl stp (ep-1) (samp_list@[q1]) pls
			else
				hmc' f pl stp (ep) (samp_list) pls



	let hmc (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) : float list list =
		let (_, smp, pls) = grad f in 
		hmc' f pl stp ep [smp] pls

	let print_sample_list ls =
		List.iter (fun ll -> print_normal_list ll) ls

	let print_values ls f =
		List.iter (fun ll-> 
			let x = get_val f ll in 
			Printf.printf "%f\n" x;
		) ls

	let get_list (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) =
		List.map (fun ll->  get_val f ll ) (hmc f pl stp ep)

	let (+.) a b = 
		perform (Add(a,b))
  	
  	let (-.) a b = 
  		perform (Sub(a,b))

  	let ( *. ) a b = 
  		perform (Mult(a,b))
  	
  	let ( /. ) a b = 
  		perform (Div(a,b))

  	
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
	let* x5 = 	x1 *. x2 *. x3 in 
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

print_endline "+++++++++++++++++++++++++++++++++++++++";
print_endline "=======================================";
print_endline "Checking A simple HMC for my DL exam";
print_endline "=======================================";
print_endline "+++++++++++++++++++++++++++++++++++++++";


(*Test 2*)
let f1 () =
	let* x1 = norm (mk 45.11363636) (mk 14.54225817) in
	let* x2 = norm (mk 37.8358209) (mk 15.56563281) in
	let* x3 = x1 /. (mk 70.0) in
	let* x4 = x2 /. (mk 70.0) in
	let* x5 = if (get(x3) > get(x4)) then x3 else x4 in
	let* x6 = if (get(x3) > get(x4)) then x4 else x3 in
	let* x7 = (mk 5. /. mk 12.) *. x6 +. (mk 7. /. mk 12.) *. x5 in
	x7 *. (mk 65.)
in

let epochs = 10000 in
let fils = Array.of_list (get_list f1 2.0 0.5 epochs) in

Printf.printf "Mean = %f\n" (Owl_stats.mean fils);
Printf.printf "Std. Dev. = %f\n\n" (Owl_stats.std fils);


print_endline "+++++++++++++++++++++++++++++++++++++++";
print_endline "=======================================";
print_endline "+++++++++++++++++++++++++++++++++++++++";



