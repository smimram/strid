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

let epsilon = 1000. *. epsilon_float

let pi = 4. *. atan 1.

let iffound f =
  try f () with Not_found -> ()

let rd_add (x, y) (dx, dy) = (x +. dx, y +. dy)
let rd_sub (x2, y2) (x1, y1) = (x2 -. x1, y2 -. y1)
type opt = string * ((string * string) list)

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

let re_box = Str.regexp "\\([0-9]+\\)box\\([0-9]+\\)"

let circle_position center point =
  let (px,py) = center in
  let (qx,qy) = point in
  let dir = (qx-.px,qy-.py) in
  let (dx,dy) = dir in
  let norm = sqrt(dx*.dx +. dy*.dy) in
  let cr = Conf.get_float "small_circle_ray" in
    (px +. 0.1*.dx/.norm *. cr, py +. 0.1*.dy/.norm *. cr)

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
    else
      let sign = sens/.(abs_float sens) in
      let scale = 0.15 *. sqrt (dx *. dx +. dy *. dy) in
      let (decalage_x,decalage_y) = (cx-.px,cy-.py) in
        circle_position center (cx +. sign *. dx -. scale *. decalage_x,
                                cy +. sign *. dy -. scale *. decalage_y)

let triangle_points pos dir height width =
  let px, py = pos in
  let dx, dy = dir in
  let ox, oy = Vect.orthogonal dir in
  let r = height /. 2. in
  let up = px +. r *. dx, py +. r *. dy in
  let left = px -. r *. dx +. width /. 2. *. ox, py -. r *. dy +. width /. 2. *. oy in
  let right = px -. r *. dx -. width /. 2. *. ox, py -. r *. dy -. width /. 2. *. oy in
    [up; left; right; up]

let middle p q =
  let xs, ys = p in
  let xt, yt = q in
    (xt -. xs) /. 2. , (yt -. ys) /. 2.

type environment = (string * string) list

class box (kind:string) (connections:Wire.reldir list) (options:opt list) =
object (self)
  val options = options

  val mutable env = []

  method set_env (e:environment) =
    env <- e

  method private get_attr name subname =
    List.assoc subname (List.assoc name options)

  method private get_attr_float name subname =
    float_of_string (self#get_attr name subname)

  method private has_attr name =
    try
      ignore (List.assoc name options); true
    with
      | Not_found -> false

  method kind = kind

  val mutable connections = Array.of_list connections

  method connections = connections

  method set_connections c = connections <- c

  method get_plines pos =
    let c = Array.map (rd_add pos) self#connections in
      match self#kind with
        | "mult" ->
            let i = Wire.new_polyline [pos; (*circle_position pos c.(2);*) c.(2)] in
            let u = Wire.new_polyline [c.(0); ortho_point pos c.(2) c.(0) c.(1); pos; ortho_point pos c.(2) c.(1) c.(0); c.(1)] in
              (*i#append_line (new Wire.line (circle_position pos c.(2)) c.(2));*)
              [i; u]
        | "arc" ->
            let u = new Wire.polyline (new Wire.line c.(0) pos) in
              u#append_line (new Wire.line pos c.(1));
              [u]
        | "sym" ->
            let p = Wire.new_polyline [c.(0); pos; c.(3)] in
            let q = Wire.new_polyline [c.(1); pos; c.(2)] in
              [p; q]
        | "braid" ->
            let p1 = Wire.new_polyline [c.(1); circle_position pos c.(1)] in
            let l = new Wire.line (circle_position pos c.(1)) (circle_position pos c.(2)) in
            let p2 = Wire.new_polyline [circle_position pos c.(2); c.(2)] in
            (* let q = Wire.new_polyline [c.(0); pos; c.(3)] in *)
            let q = Wire.new_polyline [c.(0); circle_position pos c.(0); circle_position pos c.(3); c.(3)] in
              l#add_attr "opacity" "0";
              p1#append_line l;
              p1#append p2;
              [p1; q]
        | "line" ->
            let l = Wire.new_polyline
                      (if Array.length c >= 2 then
                         [c.(0); pos; c.(1)]
                       else
                         [c.(0); pos])
            in
              l::[]
        | "adj" ->
            [Wire.new_polyline [c.(0); pos]; Wire.new_polyline [pos; c.(1)]]
        | "unit" ->
            [Wire.new_polyline [pos; c.(0)]]
        | "text" -> []
        | "region" -> []
        | "vbox" ->
            let px, _ = pos in
            let u = Array.map (fun (x,y) -> Wire.new_polyline [x,y; px+.x*.epsilon,y]) c in
              Array.to_list u
        | "operad" ->
            let i = Array.length c - 1 in
            let px, py = pos in
            let ans = ref [] in
            let height = deffound (Conf.get_float "label_triangle_height") (fun () -> self#get_attr_float "l" "h") in
            let width = deffound (Conf.get_float "label_triangle_height" *. 2. /. sqrt 3.) (fun () -> self#get_attr_float "l" "w") in
            let dx,dy = Vect.normalize (rd_sub c.(i) pos) in
            let ox,oy = Vect.normalize (orthogonal c.(i) pos) in
              for n = 0 to i - 1 do
                let pl =
                  Wire.new_polyline
                    [
                      c.(n);
                      px +. ox *. width *. ((float_of_int n +. 1.) /. (float_of_int i +. 1.)  -. 1. /. 2.)
                      -. dx *. height /. 2.,
                      py +. oy *. width *. ((float_of_int n +. 1.) /. (float_of_int i +. 1.)  -. 1. /. 2.)
                      -. dy *. height /. 2.
                    ]
                in
                  ans := pl::!ans
              done;
              let pl = Wire.new_polyline [pos; c.(i)]
              in
                ans := pl::!ans;
                !ans
        | k when Str.string_match re_box k 0 ->
            let i = int_of_string (Str.matched_group 1 k) in
            let o = int_of_string (Str.matched_group 2 k) in
            let px, py = pos in
            let ans = ref [] in
              for n = 0 to i - 1 do
                let pl = Wire.new_polyline [c.(n); (*circle_position pos c.(n);*) px +. (float_of_int n) *. epsilon, py]
                in
                  ans := pl::!ans
              done;
              for n = i to i + o - 1 do
                let pl = Wire.new_polyline [px +. (float_of_int n) *. epsilon, py; (*circle_position pos c.(n);*) c.(n)]
                in
                  ans := pl::!ans
              done;
              !ans
        | k ->
            warning (Printf.sprintf "Don't know lines for %s box." k); []

  method get_label_decorations pos =
    let c = Array.map (rd_add pos) self#connections in
      match self#kind with
        | "text" -> []
        | "region" ->
            let r = new Wire.rectangle pos c.(0) in
              r#add_attr "style" "dashed";
              [r]
        | "unit" when not (self#has_attr "l") ->
            [new Wire.ellipse pos (0.14, 0.14)]
        | "vbox" ->
            let dx = (deffound (Conf.get_float "label_rectangle_width") (fun () -> self#get_attr_float "l" "w")) /. 2. in
            let dy = (deffound (Conf.get_float "label_rectangle_height") (fun () -> self#get_attr_float "l" "h")) /. 2. in
            let px, py = pos in
            let y, y' = Array.fold_left (fun (y,y') (_,cy) -> min y cy, max y' cy) (py,py) c in
            let r = new Wire.rectangle (px-.dx,y-.dy) (px+.dx,y'+.dy) in
              r#add_attr "color" (deffound "white" (fun () -> self#get_attr "l" "s"));
              [r]
        | _ ->
            deffound []
              (fun () ->
                 let () =
                   if self#kind <> "operad" then
                     (* Do we have a label? *)
                     ignore (List.assoc "l" options)
                   else
                     ()
                 in
                 let shape =
                   deffound
                     (if self#kind = "operad" then
                        (* The default shape for operads is a triangle. *)
                        "triangle"
                      else
                        "ellipse")
                     (fun () -> self#get_attr "l" "s")
                 in
                   match shape with
                     | "r"
                     | "rectangle" ->
                         let dx = (deffound (Conf.get_float "label_rectangle_width") (fun () -> self#get_attr_float "l" "w")) /. 2. in
                         let dy = (deffound (Conf.get_float "label_rectangle_height") (fun () -> self#get_attr_float "l" "h")) /. 2. in
                         let x,y = pos in
                         let e = new Wire.polygon [x-.dx,y-.dy; x-.dx,y+.dy; x+.dx,y+.dy; x+.dx,y-.dy; x-.dx,y-.dy] in
                           iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
                           [e]
                     | "t"
                     | "triangle" ->
                         let height = deffound (Conf.get_float "label_triangle_height") (fun () -> self#get_attr_float "l" "h") in
                         let width = deffound (Conf.get_float "label_triangle_height" *. 2. /. sqrt 3.) (fun () -> self#get_attr_float "l" "w") in
                         (* direction *)
                         let dir =
                           deffound
                             (if self#kind = "operad" then
                                (* We can guess the direction for operads. *)
                                Vect.normalize (rd_sub c.(Array.length c - 1) pos)
                              else
                                Vect.normalize (reldir_of_string "u"))
                             (fun () -> Vect.normalize (reldir_of_string (self#get_attr "l" "d")))
                         in
                         let e = new Wire.polygon (triangle_points pos dir height width) in
                           iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
                           [e]
                     | "e"
                     | "ellipse" ->
                         let xray = deffound (Conf.get_float "label_width") (fun () -> self#get_attr_float "l" "w") in
                         let yray = deffound (Conf.get_float "label_height") (fun () -> self#get_attr_float "l" "h") in
                         let e = new Wire.ellipse pos (xray, yray) in
                           iffound (fun () -> e#add_attr "border width" (self#get_attr "l" "b"));
                           iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
                           [e]
                     | "n"
                     | "none" ->
                         []
                     | _ ->
                         warning (Printf.sprintf "Unknown label shape: %s." shape); []
              )

  method get_texts pos =
    let c = Array.map (rd_add pos) self#connections in
      match self#kind with
        | "text" ->
            deffound []
              (fun () ->
                 let label = List.assoc "l" options in (* value *)
                 let t = List.assoc "t" label in (* text *)
                 let px, py = pos in
                 let pxt, pyt =
                   if Array.length c >= 1 then c.(0) else 0., 0.
                 in
                 let p = (px +. pxt) /. 2., (py +. pyt) /. 2. in
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
  let ldeco = ref [] in
  let texts = ref [] in
  let add_box pos b =
    match b with
      | None -> ()
      | Some b ->
          debug (Printf.sprintf "New %s box." b#kind);
          plines := !plines@b#get_plines pos;
          ldeco := !ldeco@b#get_label_decorations pos;
          texts := !texts@b#get_texts pos
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
           let params =
             (if (Conf.get_float "xscale") = 1. then [] else [Printf.sprintf "xscale=%.02f" (Conf.get_float "xscale")])
             @(if (Conf.get_float "yscale") = 1. then [] else [Printf.sprintf "yscale=%.02f" (Conf.get_float "yscale")])
             @(if (Conf.get_float "scaling_factor") = 1. then [] else [Printf.sprintf "scale=%.02f" (Conf.get_float "scaling_factor")])
             @(if (Conf.get_string "line_width") = "0.5pt" then [] else [Printf.sprintf "line width=%s" (Conf.get_string "line_width")])
           in
           let params = String.concat "," params in
           let params = if params = "," then "" else "[" ^ params ^ "]" in
             (if Conf.get_bool "no_tex_environment" then
                ""
              else
                "\\begin{tikzpicture}"
                ^ params
                ^ "\n")
             ^ (Printf.sprintf "\\useasboundingbox (-0.5,-0.5) rectangle (%d.5,%d.5);\n" !width height)
       | Wire.Pstricks ->
           Printf.sprintf "\\begin{pspicture}(0,0)(%d,%d)\n" !width height
       | Wire.Graphics ->
           Graphics.open_graph "";
           Graphics.resize_window ((max !width 1)*50) ((max height 1)*50);
           Graphics.set_window_title "Strid";
           ""
    );
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s\n" (pl#draw kind)) !plines;
    List.iter (fun e -> out := !out ^ Printf.sprintf "%s\n" (e#draw kind)) !ldeco;
    List.iter (fun t -> out := !out ^ Printf.sprintf "%s\n" (t#draw kind)) !texts;
    out := !out ^
    (match kind with
       | Wire.Tikz ->
           if (Conf.get_bool "no_tex_environment") then "" else "\\end{tikzpicture}\n"
       | Wire.Pstricks -> "\\end{pspicture}\n"
       | Wire.Graphics -> ""
    );
    !out
