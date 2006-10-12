(** Do [f], returning a default value if [Not_found] is raised. *)
let deffound v f =
  try f () with Not_found -> v

(** Norm of the vector p1 p2. *)
let length p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
    sqrt((x2-.x1)*.(x2-.x1) +. (y2-.y1)*.(y2-.y1))

(* Remove duplicate coordinates. *)
let rec remove_consecutive_dups = function
  | p::q::t when p = q -> remove_consecutive_dups (q::t)
  | p::q::t -> p::(remove_consecutive_dups (q::t))
  | l -> l

let queue_of_list l =
  let ans = Queue.create () in
    List.iter (fun x -> Queue.push x ans) l;
    ans
