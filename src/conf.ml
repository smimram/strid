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

let default_conf = ["label_width", "0.8"; "label_height", "0.5"]

let conf = Hashtbl.create 100

let () =
  List.iter (fun (n,v) -> Hashtbl.add conf n v) default_conf

let save () =
  Hashtbl.iter
    (fun n v ->
       Printf.printf "%s = %s\n%!" n v
    ) conf
