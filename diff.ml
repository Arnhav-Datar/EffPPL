exception Unknown

module AD : sig
  
  type t
  val mk : float -> t
  val (+.) :  t -> t -> t
  val (-.) :  t -> t -> t
  val ( *. ) : t ->  t ->  t
  val ( /. ) : t ->  t ->  t
  val ( let* ) :  t -> (t -> t ) ->  t
  val grad  : ( unit ->  t) -> (t list * float list)
  val samp :float Primitive.t ->  t
  val get : t -> float
  val get_val : ( unit ->  t) -> float list -> float
  val get_der : ( unit ->  t) -> float list -> (float * t list)
  val cond: bool -> t -> t -> t
  val hmc :  ( unit ->  t)  -> float -> float -> int -> float list list
  val print_list : t list -> unit
  val print_normal_list : float list -> unit
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
	effect Leet : t * (t -> t) -> t
	effect Samp : float Primitive.t ->  t
	
	
	(* let rec find_list v ls = 
		match ls with 
		| [] -> 
			raise Unknown
		| {v = v1; d= d1; m=_}::tl -> 
			if(v=v1) then d1 else find_list v tl *)

  	let modif_der ls vc dc = 
  		(* Printf.printf "Modifying %f to %f\n" vc dc; *)
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
  		) ls

  	let rec run_grad f ls sls =
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
			let (x1) = (run_grad (fun () -> f' m) ls sls) in
			(x1)

		| effect (Samp(p)) k ->
			(* print_endline "in samp"; *)
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			ls := modif_der !ls v1 (x.d *. v2);
			(x)
		
  	let grad f =
  		let rls = ref [] in 
  		let sls = ref [] in 
		let _ = run_grad f rls sls in 
		(!rls,!sls)


	let rec get_val' f ls = 
		match f () with 
		| r -> 
			r

		| effect (Samp(_)) k ->
			print_endline "in samp";
			match !ls with 
			| [] -> raise Unknown 
			| hd::tl ->
				Printf.printf "%f \n" hd;
				ls := tl;
				let x = {v = hd; d = 0.; m=1.} in
				ignore (continue k x) ;
				x

		| effect (Add(a,b)) k ->
			print_endline "in add";
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x) ;
			x
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k x) ;
			x

		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.; m=1.} in
			ignore (continue k x) ;
			x
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.; m=1.} in
			ignore (continue k x) ;
			x
		
		| effect (Leet(m,f')) _ ->
			print_endline "in let";
			let x1 = get_val' (fun () -> f' m) ls in
			x1
		
	let get_val f ls =
		let x1 = ls in
		let x2 = ref x1 in
		get (get_val' f x2)
	
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
		
		| effect (Samp(p)) k ->
			match !ls' with 
			| [] -> raise Unknown
			| v1::tl -> 
				let v2 = Primitive.logder p v1 in
				let x = {v=v1; d=0.0 ; m= v2 } in
				ls' := tl;
				let (rv,_) =  (continue k x) in
				ls := modif_der !ls v1 (x.d *. v2);
				(rv, ls)



	let get_der f sls =
		let rls = ref [] in
		let rsls = ref sls in 
		let (x1, _) = ( get_der' f rsls rls ) in
		(x1.v, !rls)

  	let samp p = 
  		perform (Samp(p))

		

	(* let getvlist ls = 
		List.map 
  		(
  			fun {v=v'; d=_; m=_} -> v'
  		)  ls

  	let getdlist ls =
		List.map 
  		(
  			fun {v=_; d=d'; m=_} ->  d'
  		)  ls  		
 *)
 	let norm_list n = 
 		List.init n (fun _ -> Primitive.sample (Primitive.normal 0. 1.)) 

 	let listadd l1 l2 mul= 
 		List.map2 (fun x y -> x +. (y *. mul) ) l1 l2 

 	let rec leapfrog (pl:float) (stp:float) (p1:float list) (q1:float list) (dVdQ:float list) =
 		if(stp < pl) then
 			(p1, q1)
 		else 
 			let p1 = listadd p1 dVdQ (stp/.2.0) in 
 			let q1 = listadd q1 p1 1.0 in 
 			let p1 = listadd p1 dVdQ (stp/.2.0) in 
 			leapfrog (pl -. stp) stp p1 q1 dVdQ 


 	let nlp _ =
 		0.0

	let rec hmc' (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) (samp_list: float list list) =
		if(ep=0) then
			samp_list
		else
			let q0 = List.nth samp_list (List.length samp_list - 1) in
			let q1 = q0 in
			let p0 = norm_list (List.length q1) in
			let p1 = p0 in
			let (_, dv) = get_der f q1 in
			let dvdq = List.map (fun f -> f.v *. (-. 1.0)) dv in 
			let (p1, q1) = leapfrog pl stp p1 q1 dvdq in
			let p1 = List.map (fun f -> f *. (-. 1.0)) p1 in 

			let p0p = nlp p0 in
			let p1p = nlp p1 in
			let q0p = nlp q0 in
			let q1p = nlp q1 in

			let tgt = q0p -. q1p in
			let adj = p1p -. p0p in 
			let acc = tgt +. adj in

			let x' = Primitive.sample (Primitive.continuous_uniform 0. 1.) in
			let x = Float.log x' in
			if (x < acc) then
				hmc' f pl stp (ep-1) samp_list@[q1]
			else
				hmc' f pl stp (ep-1) samp_list@[q0]


			


	let hmc (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) : float list list =
		let (_, smp) = grad f in 
		hmc' f pl stp ep [smp]


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

open AD

(* let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x2 = samp Primitive.(normal 4. 1.) in
	let* x3 = samp Primitive.(continuous_uniform 0. 1.)  in
	let* x4 = cond ((get x3) > 0.5) x1 x2 in
	x4  *)
 
(* let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x4 = log x1 in
	x4 *)

let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x2 = samp Primitive.(normal 0. 1.) in
	let* x3 = samp Primitive.(normal 0. 1.) in
	let* x4 = x1 +. x2 +. x3 in
	let* x5 = x1 *. x2 *. x3 in
	x4 +. x5

;;

let (ls,smp) =  grad f1  in
let (x,ls1) =  get_der f1 smp in
Printf.printf "%f \n" x;
print_list ls;
print_list ls1;
