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
  val get_val : ( unit ->  t) -> t list -> float
  val get_der : ( unit ->  t) -> float list -> t list
  val cond: bool -> t -> t -> t
  (* val hmc :  ( unit ->  t)  -> float -> float -> int -> float list *)
  (* path_len -> step_size -> epochs -> samples *)

end = 

struct

	type t = { v : float;  mutable d : float ; m : float}

	let mk v = 
		{v; d = 0.0; m=1.0}
	
	let get t' = 
		match t' with 
			{v= v' ; d = _ ;  m = _} -> v'

	effect Add : t * t -> (t* (t list))
	effect Sub : t * t -> (t* (t list))
	effect Mult : t * t -> (t* (t list))
	effect Div : t * t -> (t* (t list))
	effect Leet : t * (t -> t) -> (t* (t list))
	effect Samp : float Primitive.t ->  (t* (t list))
	
	
	let rec find_list v ls = 
		match ls with 
		| [] -> 
			raise Unknown
		| {v = v1; d= d1; m=_}::tl -> 
			if(v=v1) then d1 else find_list v tl

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
			let ls1 = modif_der !ls r.v r.d in
			(r,ls1,!sls)
		
		| effect (Add(a,b)) k ->
			(* print_endline "in add"; *)
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x,!ls,!sls)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x,!ls,!sls)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x,!ls,!sls)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x,!ls,!sls)
		
		| effect (Leet(m,f')) _ ->
			(* print_endline "in let"; *)
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1,ls1, sls1) = (run_grad (fun () -> f' m) ls sls) in
			let d' = find_list x.v ls1 in 
			x.d <- d';
			(x1,!ls,sls1)

		| effect (Samp(p)) k ->
			(* print_endline "in samp"; *)
			let v1 = Primitive.sample p in
			sls := !sls@[v1];
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k (x,!ls));
			let ls1 = modif_der !ls v1 (x.d *. v2) in
			(x, ls1, !sls)
		
  	let grad f =
  		let rls = ref [] in 
  		let sls = ref [] in 
		let (_,ls, sls) = run_grad f rls sls in 
		print_list ls ;
		print_normal_list sls ;
		(ls,sls)



	let rec get_val' f ls = 
		match f () with 
		| r -> 
			(* Printf.printf "in r %f \n" r.v; *)
			r

		| effect (Samp(_)) k ->
			(* print_endline "in samp"; *)
			let x = {v = 0.0; d = 0.; m=1.} in
			let x1 = (continue k (x,ls)) in 
			x1

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			let x1 = (continue k (x,ls)) in 
			x1
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			let x1 = (continue k (x,ls)) in 
			x1

		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.; m=1.} in
			let x1 = (continue k (x,ls)) in 
			x1
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.; m=1.} in
			let x1 = (continue k (x,ls)) in 
			x1
		
		| effect (Leet(_,f')) _ ->
			match ls with 
			| [] -> raise Unknown 
			| hd::tl ->
				let v1 = (get_val' (fun () -> f' hd) (tl)) in 
				v1
		
	let get_val f ls =
		get (get_val' f ls)
	
	let rec get_der' f ls' ls = 
		match f () with 
		| r -> 
			r.d <- 1.0; 
			ls := modif_der !ls r.v r.d;
			(r,ls)
		
		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x, ls)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x, ls)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x, ls)
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k (x,!ls));
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			ls := modif_der !ls a.v a.d;
			ls := modif_der !ls b.v b.d;
			(x, ls)
		
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
				ignore (continue k (x,!ls));
				ls := modif_der !ls v1 (x.d *. v2);
				(x, ls)



	let get_der f sls =
		let rls = ref [] in
		let rsls = ref sls in 
		ignore( get_der' f rsls rls );
		print_list !rls;
		!rls

  	let samp p = 
  		let (x,_) = perform (Samp(p)) in x

		

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

	let hmc' (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) (samp_list: float list) =
		if (ep=0) then 
			samp_list
		else
			samp_list *)


	(* let hmc (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) : float list =
		let ls = grad f in 
		hmc' f pl stp ep [] 	 *)	






	let (+.) a b = 
  		let (x,_) = perform (Add(a,b)) in x
  	
  	let (-.) a b = 
  		let (x,_) = perform (Sub(a,b)) in x

  	let ( *. ) a b = 
  		let (x,_) = perform (Mult(a,b)) in x

  	
  	let ( /. ) a b = 
  		let (x,_) = perform (Div(a,b)) in x

  	
  	let (let*) m f = 
  		let (x,_) = perform (Leet(m,f)) in x

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
	let* x4 = x2 -. x3 in
	let* x5 = x1 +. x4 in
	x5

;;

let (ls,smp) =  grad f1  in
print_endline " "; 
let v1 =  get_val f1 ls in
Printf.printf "%f \n" v1;
print_endline " "; 
ignore( get_der f1 smp );
(* ignore (grad f1); *)

