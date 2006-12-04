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

open Common

(*
(** Interpolation type. *)
  let interpolation_type = Gsl_interp.CSPLINE

(** Compute the interpolation. *)
  let rec interpolation_rec steps point_list courbe =
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
  let interpolation periodic steps point_list =
  let fst_array = Array.of_list (List.map fst point_list) in
  let snd_array = Array.of_list (List.map snd point_list) in
  let courbe = Gsl_interp.make_interp
  (if periodic then Gsl_interp.CSPLINE_PERIODIC else Gsl_interp.CSPLINE)
  fst_array snd_array
  in
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
  let compute ?(periodic = false) steps points =
  let interpolation_x = interpolation periodic steps (split_function_rec fst points 0.) in
  let interpolation_y = interpolation periodic steps (split_function_rec snd points 0.) in
  List.map2 (fun x y -> (fst x,(snd x,snd y))) interpolation_x interpolation_y
*)

(** Compute the interpolation. *)
let rec interpolation_rec steps point_list courbe =
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
                    partial_list := !partial_list@[(!t,Cubic_spline.eval courbe !t)];
                    t := !t +. (time2-.time) /. (float_of_int steps);
                  done;
                  !partial_list@interpolation_rec steps q courbe

(** Compute the interpolation. *)
let interpolation periodic steps point_list =
  let fst_array = Array.of_list (List.map fst point_list) in
  let snd_array = Array.of_list (List.map snd point_list) in
  let courbe = Cubic_spline.make_interp fst_array snd_array periodic
  in
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
let compute ?(periodic = false) steps points =
  let interpolation_x = interpolation periodic steps (split_function_rec fst points 0.) in
  let interpolation_y = interpolation periodic steps (split_function_rec snd points 0.) in
    List.map2 (fun x y -> (fst x,(snd x,snd y))) interpolation_x interpolation_y
