

(* square function *)
let square x =
  x *. x

(* average function *)
let average x y =
  (x +. y) /. 2.

(* improve the guess *)
let improve_guess guess x =
  average guess (x /. guess)

(* abs function *)
let abs_func (x:float) :float =
  if x >= 0. then x else  -.x

(* good enough function *)
let good_enough guess x =
  ((abs_func ((square guess) -. x)) > 0.001)

(* square-iteration function *)
let rec square_iter guess x =
  if (good_enough guess x) then guess
  else square_iter (improve_guess guess x) x
