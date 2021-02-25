exception Unknown

module AD : sig
  
  type t
  val return : float -> t
  val (+.) :  t -> t -> t
  val ( *. ) : t ->  t ->  t
  val ( let* ) :  t -> (t -> t ) ->  t
  val grad  : ( unit ->  t) -> float
  val samp : float Primitive.t ->  t

end = 

struct

  type t = { v : float;  mutable d : float ; m : float}

  let return v = {v; d = 0.0; m=1.0}

  effect Add : t * t -> t
  (* effect Sub :  *)
  effect Mult : t * t -> t
  effect Leet : t * (t -> t) -> t

  let rec find_list v ls= match ls with 
  | [] -> raise Unknown
  | {v = v1; d= d1; m=_}::tl -> if(v=v1) then d1 else find_list v tl

  let rec modif_der ls vc dc = match ls with 
  | [] -> []
  | {v = v1; d= d1; m=m1}::tl -> if(vc=v1) then {v= vc;d= dc; m=m1}::modif_der tl vc dc else {v= v1;d= d1; m=m1}::modif_der tl vc dc	

  let rec print_list ls = match ls with 
  | [] -> print_endline ""
  | {v = v1; d= d1; m=m1}::tl -> Printf.printf "%f %f " v1 (d1*.m1); print_list tl

  let rec run f ls =
    match f () with
    | r -> 
    	r.d <- 1.0; 
    	(* print_endline "Iden";  *)
        ls := modif_der !ls r.v r.d; 
        r
    | effect (Add(a,b)) k ->
        let x = {v = a.v +. b.v; d = 0.; m=1.} in
        ignore (continue k x);
        (* print_list !ls; *)
        a.d <- a.d +. x.d;
        ls := modif_der !ls a.v a.d;
        b.d <- b.d +. x.d; 
        ls := modif_der !ls b.v b.d;
        x
    | effect (Mult(a,b)) k ->
        let x = {v = a.v *. b.v; d = 0.;  m=1.} in
        ignore (continue k x);
        a.d <- a.d +. (b.v *. x.d);
        ls := modif_der !ls a.v a.d;
        b.d <- b.d +. (a.v *. x.d);
        ls := modif_der !ls b.v b.d;
        x
    | effect (Leet(m,f')) _ ->
        let x = {v = m.v; d = 0.0; m=m.m} in 
    	(* print_endline "In let";
    	Printf.printf "value = %f\n" m.v;
    	Printf.printf "diff = %f\n" m.d; *)
    	ls := !ls @ [x] ;
    	(* print_list !ls; *)
    	(* Printf.printf "------------------\n"; *)
    	let _ = (run (fun () -> f' m) ls) in
    	(* print_list !ls; *)
    	let d' = find_list x.v !ls in 
    	(* Printf.printf "overall diff = %f\n" d' ; *)
    	x.d <- d';
    	x

  let grad f=
  	let ls = ref [] in
    let x1 = run f ls in 
    print_list !ls;
    x1.d


	 
  let (+.) a b = perform (Add(a,b))
  let ( *. ) a b = perform (Mult(a,b))
  let (let*) m f = perform (Leet(m,f))


  let samp p = 
    let v1 = Primitive.sample p in
    let v2 = Primitive.der p v1 in
    (* Printf.printf "Sampling %f %f\n" v1 v2; *)
    {v = v1;  d= 0.; m=v2}
    (* let s = 1. in
    let m = 0. in
    let v1 = Owl_stats.gaussian_rvs ~mu:m ~sigma:s in
    let v2 = Owl_stats.gaussian_pdf v1  ~mu:m ~sigma:s  in
    let v3 = mk v1 in
    v3.d <- v2;
    v3.d <- 1.;
    v3 *)

end;;

open AD

let f1 () = 
	let* x1 = samp Primitive.(normal 0. 1.) in
	let* x2 = samp Primitive.(normal 0. 1.) in
	let* x3 = x1 *. x2 in
	x3 
	(* let* x4 = x1 *. x2 in *)
	(* return(2.) *. (x3 +. x4)  *)

;;

ignore ( grad f1 )