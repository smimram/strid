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

let debug = Printf.printf "[DD] %s\n%!"
let info = Printf.printf "[II] %s\n%!"
let warning = Printf.printf "[WW] %s\n%!"
let error e = Printf.printf "[EE] %s\n%!" e; exit 1

let ellipse_X_ray = ref 1.2
let ellipse_Y_ray = ref 0.7
let circle_ray = ref 1.2
let pi = 4.*. (atan 1.)

let iffound f =
  try f () with Not_found -> ()

let rd_add (x, y) (dx, dy) = (x +. dx, y +. dy)
type opt = string * ((string * string) list)

let re_box = Str.regexp "\\([0-9]+\\)box\\([0-9]+\\)"

let circle_position center point =
  let (px,py) = center in
  let (qx,qy) = point in
  let dir = (qx-.px,qy-.py) in
  let (dx,dy) = dir in
  let norm = sqrt(dx*.dx +. dy*.dy) in
    (px +. 0.1*.dx/.norm *. !circle_ray , py +. 0.1*.dy/.norm *. !circle_ray)

let orthogonal center point = 
  let (px,py) = center in
  let (qx,qy) = point in
  let dir = (qx-.px,qy-.py) in
  let (dx,dy) = dir in
    (dy,-.dx)

let ortho_point center point dir dir2 =
  let (cx,cy) = center in
  let (px,py) = dir in
  let (dx,dy) = orthogonal center point in
  let (qx,qy) = dir2 in
  let (dx,dy) =
    begin
      if ((dx = 0.) && (dy = 0.))
      then (qx-.px,qy-.py)
      else (dx,dy);
    end;
  in
  let sens = (px-.cx)*.dx+.(py-.cy)*.dy in
    if sens = 0. then center
    else let sign = sens/.(abs_float sens) in
      circle_position center (cx +. sign *. dx, cy +. sign *. dy)

let middle p q =
  let xs, ys = p in
  let xt, yt = q in
    (xt -. xs) /. 2. , (yt -. ys) /. 2.

class box (kind:string) (connexions:Wire.reldir list) (options:opt list) =
object (self)
  val options = options

  val mutable env = []

  method set_env (e:(string * string) list) =
    env <- e

  method get_attr name subname =
    List.assoc subname (List.assoc name options)

  method kind = kind

  method connexion = Array.of_list connexions

  method get_plines pos =
    let c = Array.map (rd_add pos) self#connexion in
      match self#kind with
        | "mult" ->
            let i = Wire.new_polyline (pos::(*(circle_position pos c.(2))::*)c.(2)::[]) in
            let u = Wire.new_polyline (c.(0)::(ortho_point pos c.(2) c.(0) c.(1))::pos::(ortho_point pos c.(2) c.(1) c.(0))::c.(1)::[]) in
              (*i#append_line (new Wire.line (circle_position pos c.(2)) c.(2));*)
              i::u::[]
        | "arc" ->
            let u = new Wire.polyline (new Wire.line c.(0) pos) in
              u#append_line (new Wire.line pos c.(1));
              [u]
        | "sym" ->
            let p = Wire.new_polyline (c.(0)::pos::c.(3)::[]) in
            let q = Wire.new_polyline (c.(1)::pos::c.(2)::[]) in
              p::q::[]
        | "braid" ->
            let p1 = Wire.new_polyline (c.(0)::(circle_position pos c.(0))::[]) in
            let l = new Wire.line (circle_position pos c.(0)) (circle_position pos c.(3)) in
            let p2 = Wire.new_polyline ((circle_position pos c.(3))::c.(3)::[]) in
            (* let q = Wire.new_polyline (c.(1)::pos::c.(2)::[]) in *)
            let q = Wire.new_polyline (c.(1)::(circle_position pos c.(1))::(circle_position pos c.(2))::c.(2)::[]) in
              l#add_attr "opacity" "0";
              p1#append_line l;
              p1#append p2;
              p1::q::[]
        | "line" ->
            let l = Wire.new_polyline (c.(0)::pos::c.(1)::[]) in
              l::[]
        | "unit" ->
            [Wire.new_polyline (pos::c.(1)::[])]
        | "text" -> []
        | k when Str.string_match re_box k 0 ->
            let i = int_of_string (Str.matched_group 1 k) in
            let o = int_of_string (Str.matched_group 2 k) in
            let px, py = pos in
            let ans = ref [] in
              for n = 0 to i - 1 do
                let pl = Wire.new_polyline (c.(n)::(*(circle_position pos c.(n))::*)(px +. (float_of_int n) *. epsilon_float, py)::[])
                in
                  ans := pl ::!ans
              done;
              for n = i to i + o - 1 do
                let pl = Wire.new_polyline ((px +. (float_of_int n) *. epsilon_float, py)::(*(circle_position pos c.(n))::*)c.(n)::[])
                in
                  ans := pl ::!ans
              done;
              !ans
        | k ->
            warning (Printf.sprintf "Don't know lines for %s box." k); []

  method get_ellipses pos =
    match self#kind with
      | "text" -> []
      | "unit" ->
          [new Wire.ellipse pos (0.2, 0.2)]
      | _ ->
          deffound []
            (fun () ->
               let _ = List.assoc "l" options in (* label *)
               let xray = deffound !ellipse_X_ray (fun () -> float_of_string (self#get_attr "l" "w")) in
               let yray = deffound !ellipse_Y_ray (fun () -> float_of_string (self#get_attr "l" "h")) in
               let e = new Wire.ellipse pos (xray, yray) in
                 iffound (fun () -> e#add_attr "border width" (self#get_attr "l" "b"));
                 [e]
            )

  method get_texts pos =
    let c = Array.map (rd_add pos) self#connexion in
      match self#kind with
        | "text" ->
            deffound [] (fun () ->
                           let label = List.assoc "l" options in (* value *)
                           let t = List.assoc "t" label in (* text *)
                           let px, py = pos in
                           let pxs, pys = c.(0) in
                           let pxt, pyt = c.(1) in
                           let p = px +. (pxt -. pxs) /. 2., py +. (pyt -. pys) /. 2. in
                             [new Wire.text p t]
            )
        | _ ->
            deffound []
              (fun () ->
                 let t = self#get_attr "l" (* label *) "t" (* text *) in
                   [new Wire.text pos t]
              )
end

type line = box option list
type ir_matrix = line list
type matrix = box option array array
type dir = Left | Right | Up | Down

let string_of_char = String.make 1

let re_dir = Str.regexp "\\([0-9\\.]*\\)\\([lrud]\\)"

let reldir_of_string s =
  let xans, yans = ref 0., ref 0. in
  let i = ref 0 in
    try
      while true do
        let m = Str.search_forward re_dir s !i in
        let n = Str.matched_group 1 s in
        let n = if n = "" then 1. else float_of_string n in
        let d = Str.matched_group 2 s in
          i := m + (String.length (Str.matched_string s));
          match d with
            | "l" -> xans := !xans -. n
            | "r" -> xans := !xans +. n
            | "u" -> yans := !yans +. n
            | "d" -> yans := !yans -. n
            | _ -> failwith ("Invalid direction " ^ (Str.matched_string s))
      done;
      !xans, !yans
    with Not_found -> !xans, !yans

let matrix_of_ir env ir =
  Array.map (fun l -> (List.iter (fun b -> match b with None -> () | Some b -> b#set_env env) l); Array.of_list l) (Array.of_list ir)

let rec join_plines plines =
  let rec find cur = function
    | [] -> []
    | h::t when cur#dst = h#src ->
        cur#append h;
        find cur t
    | h::t when cur#src = h#dst ->
        cur#prepend h;
        find cur t
    | h::t when cur#dst = h#dst ->
        cur#append (h#rev);
        find cur t
    | h::t when cur#src = h#src ->
        cur#prepend (h#rev);
        find cur t
    | h::t ->
        h::(find cur t)
  in
    match plines with
      | [] -> []
      | h::t -> let tl = find h (join_plines t) in h::tl

let process_matrix kind env m =
  let out = ref "" in
  let plines = ref [] in
  let ellipses = ref [] in
  let texts = ref [] in
  let add_box pos b =
    match b with
      | None -> ()
      | Some b ->
          debug (Printf.sprintf "New %s box." b#kind);
          plines := !plines@b#get_plines pos;
          ellipses := !ellipses@b#get_ellipses pos;
          texts := !texts@b#get_texts pos;
  in
  let height = Array.length m - 1 in
  let width = ref 0 in
    for i = 0 to height do
      let w = Array.length m.(i) - 1 in
        if w > !width then width := w;
        for j = 0 to w do
          add_box (float_of_int j, float_of_int (height - i)) m.(i).(j)
        done
    done;
    out :=
    (match kind with
       | Wire.Tikz ->
           (if !Env.no_tex_environment then
              ""
            else
              "\\begin{tikzpicture}"
              ^ (if !Env.scaling_factor = 1. then "" else Printf.sprintf "[scale=%.02f]" !Env.scaling_factor)
              ^ "\n")
           ^ (Printf.sprintf "\\useasboundingbox (0,0) rectangle (%d,%d);\n" (!width + 1) height)
       | _ -> Printf.sprintf "\\begin{pspicture}(0,0)(%d,%d)\n" (!width + 1) height
    );
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s\n" (pl#draw kind)) !plines;
    List.iter (fun e -> out := !out ^ Printf.sprintf "%s\n" (e#draw kind)) !ellipses;
    List.iter (fun t -> out := !out ^ Printf.sprintf "%s\n" (t#draw kind)) !texts;
    out := !out ^
    (match kind with
       | Wire.Tikz ->
           if !Env.no_tex_environment then "" else "\\end{tikzpicture}\n"
       | _ ->
           "\\end{pspicture}\n"
    );
    !out
