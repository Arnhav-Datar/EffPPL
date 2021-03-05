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
  val cond: bool -> t -> t -> t
  (* val log : t -> t *)
  (* val normal : t -> t -> t *)

end = 

struct

  type t = { v : float;  mutable d : float ; m : float}

  let mk v = {v; d = 0.0; m=1.0}
  let get t' = match t' with {v= v' ; d = _ ;  m = _} -> v'

  effect Add : t * t -> t
  effect Sub : t * t -> t
  effect Mult : t * t -> t
  effect Div : t * t -> t
  (* effect Log : t -> t *)
  effect Leet : t * (t -> t) -> t

  let rec find_list v ls= match ls with 
  | [] -> raise Unknown
  | {v = v1; d= d1; m=_}::tl -> if(v=v1) then d1 else find_list v tl

  let modif_der ls vc dc = 
  	List.map 
  		(fun {v=v'; d=d'; m=m'} -> 
  			if(v'=vc) then  {v=v'; d=dc; m=m'}
  			else {v=v'; d=d'; m=m'}
  		)  ls

  let rec print_list ls vl = match ls with 
  | [] -> print_endline ""
  | {v = v1; d= d1; m=m1}::tl -> Printf.printf "%f %f " v1 (d1*.m1); (print_list tl vl)

  let rec run_grad f ls =
	match f () with
	| r -> 
		r.d <- 1.0; 
		let ls1 = modif_der ls r.v r.d in
		(r,ls1)
	
	| effect (Add(a,b)) k ->
		let x = {v = a.v +. b.v; d = 0.; m=1.} in
		ignore (continue k x);
		a.d <- a.d +. x.d;
		b.d <- b.d +. x.d; 
		let ls1 = modif_der ls a.v a.d in
		let ls2 = modif_der ls1 b.v b.d in
		(x,ls2)
	
	| effect (Sub(a,b)) k ->
		let x = {v = a.v -. b.v; d = 0.; m=1.} in
		ignore (continue k x);
		a.d <- a.d +. x.d;
		b.d <- b.d -. x.d; 
		let ls1 = modif_der ls a.v a.d in
		let ls2 = modif_der ls1 b.v b.d in
		(x,ls2)
	
	| effect (Mult(a,b)) k ->
		let x = {v = a.v *. b.v; d = 0.;  m=1.} in
		ignore (continue k x);
		a.d <- a.d +. (b.v *. x.d);
		b.d <- b.d +. (a.v *. x.d);
		let ls1 = modif_der ls a.v a.d in
		let ls2 = modif_der ls1 b.v b.d in
		(x,ls2)	
	
	| effect (Div(a,b)) k ->
		let x = {v = a.v /. b.v; d = 0.;  m=1.} in
		ignore (continue k x);
		a.d <- a.d +. (x.d /. b.v);
		b.d <- b.d +. (a.v *. x.d /. (b.v *. b.v));
		let ls1 = modif_der ls a.v a.d in
		let ls2 = modif_der ls1 b.v b.d in
		(x,ls2)
	
	| effect (Leet(m,f')) _ ->
		let x = {v = m.v; d = 0.0; m=m.m} in 
		(* Printf.printf "%f \n" m.v; *)
		let (x1,ls1) = (run_grad (fun () -> f' m) (ls@[x])) in
		(* print_list ls1; *)
		let d' = find_list x.v ls1 in 
		x.d <- d';
		(x1,ls1)

	(* | effect (Log(a)) k ->
		(* print_string "Hello"; *)
		let x = {v = (Float.log a.v) ; d = 0.;  m=1.} in
		Printf.printf "a.v = %f \n" a.v;
		ignore (continue k x);
		a.d <- a.d +. (x.d /. a.v);
		let ls1 = modif_der ls a.v a.d in
		(x,ls1)	 *)
	
		
  	let grad f =
		let (x1,ls) = run_grad f [] in 
		print_list ls x1.v;
		ls




	let rec get_val f ls = 
	match f () with 
	| r -> 
		r.v

	| effect (Add(a,b)) _ ->
		a.v +. b.v
	
	| effect (Sub(a,b)) _ ->
		a.v -. b.v
	
	| effect (Mult(a,b)) _ ->
		a.v *. b.v	
	
	| effect (Div(a,b)) _ ->
		a.v /. b.v
	
	| effect (Leet(_,f')) _ ->
		match ls with 
		| [] -> raise Unknown 
		| hd::tl ->
			let v1 = (get_val (fun () -> f' hd) (tl)) in 
			v1
 
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

  	(* let norm mean std = 
  		let x = Primitive.sample (normal mean std) in
		let d' = Primitive.der (normal mean std) v1 in
		let y = {v=x; d=0.0; m=d'}
		perform  (Normal(mean, std, y))	 *)	


	let samp p = 
		let v1 = Primitive.sample p in
		let v2 = Primitive.logder p v1 in
		(* Printf.printf "%f\n" v2; *)
		{v=v1; d=0.0 ; m= (-.v2) }

end;;

open AD

(* let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x2 = samp Primitive.(normal 4. 1.) in
	let* x3 = samp Primitive.(continuous_uniform 0. 1.)  in
	let* x4 = cond ((get x3) > 0.5) x1 x2 in
	x4 *) 
 
(* let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x4 = log x1 in
	x4 *)

let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x2 = samp Primitive.(normal 0. 1.) in
	let* x3 = x1 /. x2 in
	x3

;;

let ls =  grad f1  in 
let v1 =  get_val f1 ls in
Printf.printf "%f " v1
