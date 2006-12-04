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

let fname = "strid.conf"

let default_conf = ["label_width", "0.8"; "label_height", "0.5"; "small_circle_ray", "2.0"]

let conf = Hashtbl.create 100

let () =
  List.iter (fun (n,v) -> Hashtbl.add conf n v) default_conf

let get =
  Hashtbl.find conf

let get_float n =
  try
    float_of_string (get n)
  with
    | _ -> Common.error (Printf.sprintf "Invalid configuration value for %s: %s" n (get n))

let set n v =
  Hashtbl.replace conf n v

let save fname =
  let oc = open_out fname in
    Hashtbl.iter
      (fun n v ->
         output_string oc (Printf.sprintf "%s = %s" n v)
      ) conf;
    close_out oc

let exists fname =
  try
    ignore (Unix.stat fname);
    true
  with
    | _ -> false

let re_conf = Str.regexp "^\\([^ =]+\\)[ ]*=[ ]*\\([^ =]+\\)"

let read fname =
  let ic = open_in fname in
  let n = ref 0 in
    try
      while true do
        let l = input_line ic in
          if not (Str.string_match re_conf l 0) then
            (
              Printf.eprintf "Configuration file %s, line %d is invalid:\n%s\n%!" fname !n l;
              exit 1
            );
          set (Str.matched_group 1 l) (Str.matched_group 2 l);
          incr n
      done
    with
      | End_of_file ->
          close_in ic
