(*
 * Copyright (C) 2006 Samuel Mimram
 *
 * This file is part of strid.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

let newline = Str.regexp "\n"

let debug _s = () (* Printf.printf "[DD] %s\n%!" s *)
let info = Printf.printf "[II] %s\n%!"
let warning = Printf.printf "[WW] %s\n%!"
let error e = Printf.printf "[EE] %s\n%!" (Str.global_replace newline "\n     " e); exit 1

(** Do [f], returning a default value if [Not_found] is raised. *)
let deffound v f =
  try f () with Not_found -> v

let iffound = deffound ()

(** Norm of the vector p1 p2. *)
let length p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  sqrt ((x2-.x1) *. (x2-.x1) +. (y2-.y1) *. (y2-.y1))

(* Remove duplicate coordinates. *)
let rec remove_consecutive_dups = function
  | p::q::t when p = q -> remove_consecutive_dups (q::t)
  | p::q::t -> p::(remove_consecutive_dups (q::t))
  | l -> l

let queue_of_list l =
  let ans = Queue.create () in
  List.iter (fun x -> Queue.push x ans) l;
  ans

let rec list_last = function
  | [] -> raise Not_found
  | [x] -> x
  | _::t -> list_last t

let get_some = function
  | Some x -> x
  | None -> assert false

let map_some f = function
  | Some x -> Some (f x)
  | None -> None

let on_some f = function
  | Some x -> f x
  | None -> ()

let rec may_map f = function
  | h::t ->
     (
       match f h with
       | Some x -> x::(may_map f t)
       | None -> may_map f t
     )
  | [] -> []

let rec map_by_2 f = function
  | x::y::t -> (f x y)::(map_by_2 f (y::t))
  | _ -> []

let may_assoc default x l =
  try
    List.assoc x l
  with
  | Not_found -> default

let float_sign x = if x < 0. then -1. else 1.

let float_approx x y = abs_float (y -. x) <= 10. *. epsilon_float

let float_approx2 (x1,y1) (x2,y2) = float_approx x1 x2 && float_approx y1 y2
