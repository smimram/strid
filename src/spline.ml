(** Interpolation type. *)
let interpolation_type = Gsl_interp.CSPLINE

(** Norm of the vector p1 p2. *)
let length p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
    sqrt((x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1))

(** Compute the interpolation. *)
let rec interpolation_rec steps point_list courbe=
  match point_list with
    | [] -> []
    | h1::q ->
        let time,pos = h1 in
          match q with
            | [] ->
                (time,pos)::interpolation_rec steps q courbe
            | h2::q2 ->
                let time2, pos12 = h2 in
                let t = ref time in
                let partial_list = ref [] in
                  for i = 1 to steps do
                    partial_list := !partial_list@[(!t,Gsl_interp.eval courbe !t)];
                    t := !t +. (time2-.time) /. (float_of_int steps);
                  done;
                  !partial_list@interpolation_rec steps q courbe

(** Compute the interpolation. *)
let interpolation steps point_list = 
  let fst_array = Array.of_list (List.map fst point_list) in
  let snd_array = Array.of_list (List.map snd point_list) in
  let courbe = Gsl_interp.make_interp interpolation_type fst_array snd_array  in 
    interpolation_rec steps point_list courbe 

(** Split the coordinates in (time,x) / (time,y). *)
let rec split_function_rec proj coordinates t =
  match coordinates with
    | [] -> []
    | h1::q ->
        match q with
          | [] -> (t, proj h1)::(split_function_rec proj q t)
          | h2::q2 -> (t, proj h1)::(split_function_rec proj q (t +. length h1 h2))

(** Compute the interpolation given an initial z coefficient, a number of steps and the
  * points. *)
let compute steps points =
  let interpolation_x = interpolation steps (split_function_rec fst points 0.) in
  let interpolation_y = interpolation steps (split_function_rec snd points 0.) in
    List.map2 (fun x y -> (fst x,(snd x,snd y))) interpolation_x interpolation_y
