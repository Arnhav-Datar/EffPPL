exception Unknown

module AD : sig
  
  type t
  val mk : float -> t
  val (+.) :  t -> t -> t
  val (-.) :  t -> t -> t
  val ( *. ) : t ->  t ->  t
  val ( /. ) : t ->  t ->  t
  val ( let* ) :  t -> (t -> t ) ->  t
  val grad  : ( unit ->  t) -> t list
  val samp :float Primitive.t ->  t
  val get : t -> float
  val get_val : ( unit ->  t) -> t list -> float
  val get_der : ( unit ->  t) -> t list -> t list
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

	effect Add : t * t -> t
	effect Sub : t * t -> t
	effect Mult : t * t -> t
	effect Div : t * t -> t
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

  	let rec run_grad f ls =
		match f () with
		| r -> 
			r.d <- 1.0; 
			let ls1 = modif_der !ls r.v r.d in
			(r,ls1)
		
		| effect (Add(a,b)) k ->
			(* print_endline "in add"; *)
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)	
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Leet(m,f')) _ ->
			(* print_endline "in let"; *)
			let x = {v = m.v; d = 0.0; m=m.m} in 
			ls := !ls@[x];
			let (x1,ls1) = (run_grad (fun () -> f' m) ls) in
			let d' = find_list x.v ls1 in 
			x.d <- d';
			(x1,ls1)

		| effect (Samp(p)) k ->
			(* print_endline "in samp"; *)
			let v1 = Primitive.sample p in
			let v2 = Primitive.logder p v1 in
			let x = {v=v1; d=0.0 ; m= v2 } in
			ignore (continue k x);
			let ls1 = modif_der !ls v1 (x.d*.v2) in
			(x, ls1)
		
  	let grad f =
  		let rls = ref [] in 
		let (_,ls) = run_grad f rls in 
		print_list ls ;
		ls



	let rec get_val' f ls = 
		match f () with 
		| r -> 
			(* Printf.printf "in r %f \n" r.v; *)
			r

		| effect (Samp(_)) k ->
			(* print_endline "in samp"; *)
			let x = {v = 0.0; d = 0.; m=1.} in
			let x1 = (continue k x) in 
			x1

		| effect (Add(a,b)) k ->
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			let x1 = (continue k x) in 
			x1
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			let x1 = (continue k x) in 
			x1

		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.; m=1.} in
			let x1 = (continue k x) in 
			x1
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.; m=1.} in
			let x1 = (continue k x) in 
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
			print_endline "in r";	
			r.d <- 1.0; 
			let ls1 = modif_der !ls r.v r.d in
			(r,ls1)
		
		| effect (Add(a,b)) k ->
			print_endline "in add";	
			let x = {v = a.v +. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d +. x.d; 
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Sub(a,b)) k ->
			let x = {v = a.v -. b.v; d = 0.; m=1.} in
			ignore (continue k x);
			a.d <- a.d +. x.d;
			b.d <- b.d -. x.d; 
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Mult(a,b)) k ->
			let x = {v = a.v *. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (b.v *. x.d);
			b.d <- b.d +. (a.v *. x.d);
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)	
		
		| effect (Div(a,b)) k ->
			let x = {v = a.v /. b.v; d = 0.;  m=1.} in
			ignore (continue k x);
			a.d <- a.d +. (x.d /. b.v);
			b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
			let ls1 = modif_der !ls a.v a.d in
			let ls2 = modif_der ls1 b.v b.d in
			(x,ls2)
		
		| effect (Leet(_,f')) _ ->
			print_endline "in let";	
			match ls' with 
			| [] -> raise Unknown 
			| hd::tl ->	 begin
				ls := !ls@[hd];
				let (x',ls1)  = (get_der' (fun () -> f' hd) tl ls) in 
				(x',ls1)	
			end
		
		| effect (Samp(_)) k ->
			print_endline "in samp";
			let x = {v=0.0; d=0.0 ; m= 0.0 } in
			let (x',ls1) = (continue k x) in
			(x', ls1)

	let get_der f ls' =
		let rls = ref [] in
		let ( _ , tl ) = get_der' f ls' rls in
		(* print_list tl; *)
		tl

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

	let hmc' (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) (samp_list: float list) =
		if (ep=0) then 
			samp_list
		else
			samp_list *)


	(* let hmc (f: ( unit ->  t) ) (pl:float) (stp:float) (ep:int) : float list =
		let ls = grad f in 
		hmc' f pl stp ep [] 	 *)	






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
	let* x3 = x1 -. x2 in
	x3

;;

let ls =  grad f1  in
print_endline " "; 
let v1 =  get_val f1 ls in
Printf.printf "%f \n" v1;
print_endline " "; 
ignore( get_der f1 ls );
(* ignore (grad f1); *)

