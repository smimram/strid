(** Initial z coefficient. *)
let z0 = ref 1.

(** Compute the next z coefficient. *)
let compute_coeff coef p1 p2  =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
    -.coef +. 2. *. (y2-.y1)/.(x2-.x1)

(** Compute the next value of the point. *)
let compute_function t time time2 coef coef2 pos pos2 =
  pos +. coef *. (t -. time) +. (coef2 -. coef) /. (2. *. (time2 -. time)) *. (t -. time) *. (t-.time)

(** Norm of the vector p1 p2. *)
let length p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
    sqrt((x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1))

(** Compute the spline. *)
let rec spline_rec steps point_list coef =
  match point_list with
    | [] -> coef, []
    | h1::q ->
        let time,pos = h1 in
          match q with
            | [] ->
                let z, spl = spline_rec steps q coef in
                  z, pos::spl
            | h2::q2 ->
                let time2, pos2 = h2 in
                let t = ref time in
                let coef2 = compute_coeff coef h1 h2 in
                let partial_list = ref [] in
                  for i = 1 to steps do
                    partial_list := !partial_list@[compute_function !t time time2 coef coef2 pos pos2];
                    t := !t +. (time2-.time) /. (float_of_int steps);
                  done;
                  let z, spl = spline_rec steps q coef2 in
                    z, !partial_list@spl

(** Compute the spline. *)
let spline steps point_list = spline_rec steps point_list !z0

(** Split the coordinates in (time,x) / (time,y). *)
let rec split_function_rec proj coordinates t =
  match coordinates with
    | [] -> []
    | h1::q ->
        match q with
          | [] -> (t, proj h1)::(split_function_rec proj q t)
          | h2::q2 -> (t, proj h1)::(split_function_rec proj q (t +. length h1 h2))

(** Compute the spline given an initial z coefficient, a number of steps and the
  * points. *)
let compute z steps points =
  let zx, spline_x = spline steps (split_function_rec fst points z) in
  let zy, spline_y = spline steps (split_function_rec snd points z) in
    (zx, zy), List.map2 (fun x y -> x, y) spline_x spline_y
