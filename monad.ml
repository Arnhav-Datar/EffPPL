(* type term = 
  | Constant of float
  | Simple of float
  | Dist of float * float
  | Oper of float * float * (float -> float -> float)

type _ dist =
  | Return : 'a -> 'a dist
  | Bind : 'a dist * ('a -> 'b dist) -> 'b dist
  | Primitive : 'a Primitive.t -> 'a dist
  | Conditional : ('a -> prob) * 'a dist -> 'a dist
  | Independent : 'a dist * 'b dist -> ('a * 'b) dist *)