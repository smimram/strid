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
open Vect

type reldir = float * float
type dir = float * float

let showpoints = ref false

type output_kind = Pstricks | Tikz | Graphics

let graphics_scale (x,y) =
  let xs, ys = Conf.get_float "xscale", Conf.get_float "yscale" in
  let xs, ys =
    let s = Conf.get_float "scaling_factor" in
      xs *. s, ys *. s
  in
    int_of_float (x *. 100. *. xs),
    int_of_float (y *. 100. *. ys)

let compl_arrow t =
  let sign = float_sign t in
  let t = abs_float t in
    sign *. (1. -. t)

class virtual wire =
object (self)
  method virtual draw : output_kind -> string

  val mutable dependencies = ([]:wire list)

  (* Specify that another wire should be drawn before. *)
  method add_dep d =
    dependencies <- d::dependencies

  method get_deps = dependencies

  val mutable attrs = ([]:(string * string) list)

  method add_attr name value =
    attrs <- (name, value)::attrs

  method add_attr_float name value =
    self#add_attr name (string_of_float value)

  method del_attr name =
    attrs <- List.filter (fun (n, v) -> n <> name) attrs

  method has_attr name =
    try
      ignore (self#get_attr name);
      true
    with
      | Not_found -> false

  method get_attr name =
    List.assoc name attrs

  method get_attrs name =
    may_map (fun (n,v) -> if n = name then Some v else None) attrs

  method get_attr_float name =
    float_of_string (self#get_attr name)

  method get_attrs_float name =
    List.map float_of_string (self#get_attrs name)

  method get_attr_d name default =
    try
      self#get_attr name
    with
      | Not_found -> default
end

class line (src:dir) (dst:dir) =
object (self)
  inherit wire

  method draw _ = failwith "Lines can't be drawn."

  method draw_arrow outkind =
    if self#has_attr "a" then
      (
        let t = self#get_attr_float "a" in
        let sign = float_sign t in
        let t = abs_float t in
        let support = dst -@ src in
        let nsupport = Vect.normalize support in
        let head = src +@ (t *@ support) in
        let head = head +@ ((sign *. Conf.get_float "arrow_length" /. 2.) *@ nsupport) in
        let base = head -@ ((sign *. (Conf.get_float "arrow_length")) *@ nsupport) in
        let o = (Conf.get_float "arrow_height" /. 2.) *@ (Vect.orthogonal nsupport) in
        let s1, s2 = base +@ o, base -@ o in
          match outkind with
            | Tikz ->
                Printf.sprintf "\\draw (%.2f,%.2f) -- (%.2f,%.2f);\n" (fst s1) (snd s1) (fst head) (snd head) ^
                Printf.sprintf "\\draw (%.2f,%.2f) -- (%.2f,%.2f);\n" (fst s2) (snd s2) (fst head) (snd head)
            | Graphics ->
                let s1 = graphics_scale s1 in
                let s2 = graphics_scale s2 in
                let head = graphics_scale head in
                  Graphics.moveto (fst s1) (snd s1);
                  Graphics.lineto (fst head) (snd head);
                  Graphics.moveto (fst s2) (snd s2);
                  Graphics.lineto (fst head) (snd head);
                  ""
            | _ -> assert false
      )
    else
      ""

  val mutable src = src

  val mutable dst = dst

  method src = src

  method dst = dst

  method length = Vect.distance src dst

  method rev =
    let tmp = src in
      src <- dst;
      dst <- tmp
end

let sp () =
  if !showpoints then "[showpoints=true]" else ""

class polyline line =
object (self)
  inherit wire

  val mutable lines = ([line] : line list)

  method lines = lines

  method src = (List.hd lines)#src

  method dst = (List.nth lines (List.length lines - 1))#dst

  method rev =
    let a = self#get_attrs_float "a" in
      self#del_attr "a";
      List.iter
        (fun t ->
           let sign = float_sign t in
           let t = abs_float t in
             self#add_attr_float "a" (-1. *. sign *. (1. -. t))
        ) a;
      lines <- List.rev lines;
      List.iter (fun l -> l#rev) lines;
      self

  method length =
    List.fold_left (+.) 0. (List.map (fun l -> l#length) self#lines)

  method append_line line =
    self#append (new polyline line)

  method prepend_line line =
    self#prepend (new polyline line)

  (** Append a polyline at the end. *)
  method append (pl:polyline) =
    assert (self#dst = pl#src);
    let s_a = self#get_attrs_float "a" in
    let s_l = self#length in
    let p_a = pl#get_attrs_float "a" in
    let p_l = pl#length in
    let l = s_l +. p_l in
      self#del_attr "a";
      List.iter
        (fun t ->
           let sign = float_sign t in
           let t = abs_float t in
           let t = t *. (s_l /. l) in
             self#add_attr_float "a" (sign *. t)
        ) s_a;
      List.iter
        (fun t ->
           let sign = float_sign t in
           let t = abs_float t in
           let t = s_l /. l +. t *. (p_l /. l) in
             self#add_attr_float "a" (sign *. t)
        ) p_a;
      lines <- lines@(pl#lines)

  (** Put a polyline before. *)
  method prepend (pl:polyline) =
    assert (self#src = pl#dst);
    let s_a = self#get_attrs_float "a" in
    let s_l = self#length in
    let p_a = pl#get_attrs_float "a" in
    let p_l = pl#length in
    let l = s_l +. p_l in
      self#del_attr "a";
      List.iter
        (fun t ->
           let sign = float_sign t in
           let t = abs_float t in
           let t = t *. (p_l /. l) in
             self#add_attr_float "a" (sign *. t)
        ) p_a;
      List.iter
        (fun t ->
           let sign = float_sign t in
           let t = abs_float t in
           let t = p_l /. l +. t *. (s_l /. l) in
             self#add_attr_float "a" (sign *. t)
        ) s_a;
      lines <- (pl#lines)@lines

  (** Split into contiguous lines with same attributes. *)
  method private split_attrs =
    let rec aux la pl = function
      (* TODO: we want a less precise equality here *)
      | h::t when h#get_attrs = la ->
          pl#append_line h;
          aux la pl t
      | h::t -> pl::(aux h#get_attrs (new polyline h) t)
      | [] -> [pl]
    in
      match lines with
        | h::t -> aux h#get_attrs (new polyline h) t
        | [] -> (* this case should not happen *) failwith "Splitting empty polyline."

  method private arrows =
    self#get_attrs_float "a"

  (** Get the code for drawing the polyline. *)
  method draw outkind =
    let resolution = ref 20 in (* number of generated points between two lines *)
    (* Remove trivial lines. *)
    let lines = List.rev (List.fold_left (fun ans l -> if l#src = l#dst then ans else (l::ans)) [] lines) in
      if lines = [] then "" else
        let points = (List.hd lines)#src::(List.map (fun l -> l#dst) lines) in
          (* let points = remove_consecutive_dups points in *)
          match points with
            | (x1,y1)::(x2,y2)::[] ->
                let arrows =
                  let ans = ref "" in
                    List.iter
                      (fun t ->
                         let l = new line (x1,y1) (x2,y2) in
                           l#add_attr_float "a" t;
                           ans := !ans ^ l#draw_arrow outkind
                      ) self#arrows;
                    !ans
                in
                  (
                    match outkind with
                      | Tikz ->
                          Printf.sprintf "\\draw (%.2f,%.2f) -- (%.2f,%.2f);\n" x1 y1 x2 y2
                      | Pstricks ->
                          Printf.sprintf "\\psline%s(%.2f,%.2f)(%.2f,%.2f)\n" (sp ()) x1 y1 x2 y2
                      | Graphics ->
                          let x1, y1 = graphics_scale (x1, y1) in
                          let x2, y2 = graphics_scale (x2, y2) in
                            Graphics.moveto x1 y1;
                            Graphics.lineto x2 y2;
                            ""
                  ) ^ arrows
            | _::[] | [] -> failwith "Drawing empty line."
            | points ->
                (
                  match Conf.get_string "interpolation" with
                    | "cspline" ->
                        let periodic = List.hd points = list_last points in
                        let spl = Spline.compute ~periodic !resolution points in
                        let spl = List.map snd spl in
                        let arrows =
                          let spln = map_by_2 Vect.distance spl in
                          let norm = List.fold_left (+.) 0. spln in
                          let spln = Array.of_list spln in
                          let spl = Array.of_list spl in
                          let ans = ref "" in
                            List.iter
                              (fun t ->
                                 let sign = float_sign t in
                                 let t = abs_float t in
                                 let n, nlen =
                                   let n = ref 0 in
                                   let len = ref 0. in
                                     while !len <= norm *. t && !n < Array.length spln - 1 do
                                       len := !len +. spln.(!n);
                                       incr n
                                     done;
                                     !n - 1, !len -. spln.(!n)
                                 in
                                 let t = t -. nlen /. norm in
                                 (* Sometimes we get very small negative values... *)
                                 let t = if t <= 0. then epsilon_float else t in
                                 let l = new line spl.(n) spl.(n+1) in
                                   l#add_attr_float "a" (sign *. t);
                                   ans := !ans ^ l#draw_arrow outkind
                              ) self#arrows;
                            !ans
                        in
                        let spl = queue_of_list spl in
                        let lines = queue_of_list lines in
                        let plast = ref (Queue.pop spl) in
                        let ans = ref "" in
                          while Queue.length spl <> 0 do
                            let l = Queue.pop lines in
                              if deffound 1. (fun () -> float_of_string (l#get_attr "opacity")) <> 0. then
                                (
                                  ans := !ans ^
                                  (match outkind with
                                     | Tikz ->
                                         Printf.sprintf "\\draw (%.2f,%.2f)" (fst !plast) (snd !plast)
                                     | Pstricks ->
                                         Printf.sprintf "\\psline%s(%.2f,%.2f)" (sp ()) (fst !plast) (snd !plast)
                                     | Graphics ->
                                         let x, y = graphics_scale !plast in
                                           Graphics.moveto x y;
                                           ""
                                  );
                                  for i = 0 to !resolution - 1 do
                                    plast := Queue.pop spl;
                                    ans := !ans ^
                                    (match outkind with
                                       | Tikz ->
                                           (* TODO: use "-- cycle" when periodic *)
                                           Printf.sprintf " -- (%.2f,%.2f)" (fst !plast) (snd !plast)
                                       | Pstricks ->
                                           Printf.sprintf "(%.2f,%.2f)" (fst !plast) (snd !plast)
                                       | Graphics ->
                                           let x, y = graphics_scale !plast in
                                             Graphics.lineto x y;
                                             ""
                                    )
                                  done;
                                  (match outkind with
                                     | Tikz ->
                                         ans := !ans ^ ";"
                                     | _ -> ()
                                  );
                                  ans := !ans ^ "\n"
                                )
                              else
                                (
                                  for i = 0 to !resolution - 1 do
                                    plast := Queue.pop spl
                                  done
                                )
                          done;
                          !ans ^ arrows
                    | "linear" ->
                        (
                          let fstpt = (List.hd lines)#src in
                            match outkind with
                              | Tikz ->
                                  Printf.sprintf "\\draw (%.2f,%.2f)" (fst fstpt) (snd fstpt) ^
                                  List.fold_left (fun s l -> let x,y = l#dst in Printf.sprintf "%s -- (%.2f,%.2f)" s x y) "" lines ^
                                  ";\n"
                              | Pstricks ->
                                  Printf.sprintf "\\plsline%s(%.2f,%.2f)" (sp ()) (fst fstpt) (snd fstpt) ^
                                  List.fold_left (fun s l -> let x,y = l#dst in
                                  Printf.sprintf "%s(%.2f,%.2f)" s x y) "" lines ^ "\n"
                              | Graphics ->
                                  let x, y = graphics_scale fstpt in
                                    Graphics.moveto x y;
                                    List.iter (fun l -> let x, y = graphics_scale l#dst in Graphics.lineto x y) lines;
                                    ""
                        )
                    | s ->
                        failwith "Unkown interpolation type: " ^ s ^ "."
                )
end

(** Create a new polyline, given a list of points. *)
let new_polyline l =
  let rec aux pl = function
    | p::q::t ->
        pl#append_line (new line p q);
        ignore (aux pl (q::t));
        pl
    | _ -> pl
  in
    match l with
      | p::q::t -> aux (new polyline (new line p q)) (q::t)
      | _ -> failwith "Trying to create an empty polyline."

class ellipse position radius =
object (self)
  inherit wire

  method draw outkind =
    let x, y = position in
    let xr, yr = radius in
      match outkind with
        | Tikz ->
            let bw =
              deffound ""
                (fun () ->
                   let bw = self#get_attr "border width" in
                     if bw = "0" then
                       ",white"
                     else
                       ",line width = " ^ bw ^ "pt"
                )
            in
              Printf.sprintf "\\filldraw[fill=white%s] (%.2f,%.2f) ellipse (%.2fcm and %.2fcm);\n" bw x y xr yr
        | Pstricks ->
            let bw =
              deffound ""
                (fun () ->
                   let bw = self#get_attr "border width" in
                     if bw = "0" then
                       ",linestyle=none"
                     else
                       ",linewidth=" ^ bw
                )
            in
              Printf.sprintf "\\psellipse[fillstyle=solid%s](%.2f,%.2f)(%.2f,%.2f)\n" bw x y xr yr
        | Graphics ->
            let x, y = graphics_scale (x, y) in
            let xr, yr = graphics_scale (xr, yr) in
              Graphics.draw_ellipse x y xr yr;
              ""
end

class rectangle corner1 corner2 =
object (self)
  inherit wire

  method draw outkind =
    let x1, y1 = corner1 in
    let x2, y2 = corner2 in
    let style = deffound "" (fun () -> self#get_attr "style") in
    let color = deffound "" (fun () -> self#get_attr "color") in
      match outkind with
        | Tikz ->
            let color = if color = "" then "" else "fill=" ^ color in
              Printf.sprintf "\\%sdraw[%s,%s] (%.2f,%.2f) rectangle (%.2f,%.2f);\n" (if color = "" then "" else "fill") style color x1 y1 x2 y2
        | Pstricks -> assert false
        | Graphics ->
            let x1, y1 = graphics_scale (x1, y1) in
            let x2, y2 = graphics_scale (x2, y2) in
              if color <> "" then
                (
                  Graphics.set_color Graphics.white;
                  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1);
                  Graphics.set_color Graphics.black;
                );
              Graphics.draw_rect x1 y1 (x2 - x1) (y2 - y1);
              ""
end

class polygon points =
object (self)
  inherit wire

  method draw outkind =
    match outkind with
      | Tikz ->
          let color = deffound "white" (fun () -> self#get_attr "color") in
          let x1, y1 = List.hd points in
            (* TODO: use -- cycle *)
            Printf.sprintf "\\filldraw[fill=%s] (%.2f, %.2f) %s;\n" color x1 y1 (List.fold_left (fun s (x,y) -> Printf.sprintf "%s -- (%.2f,%.2f)" s x y) "" (List.tl points))
      | Pstricks -> assert false
      | Graphics ->
          let points = List.map graphics_scale points in
          let points = Array.of_list points in
            Graphics.set_color Graphics.white;
            Graphics.fill_poly points;
            Graphics.set_color Graphics.black;
            Graphics.draw_poly points;
            ""
end

class text position text =
object (self)
  inherit wire

  method draw outkind =
    let x, y = position in
      match outkind with
        | Tikz ->
            Printf.sprintf "\\draw (%.2f,%.2f) node{%s};\n" x y text
        | Pstricks ->
            Printf.sprintf "\\rput(%.2f,%.2f){%s}\n" x y text
        | Graphics ->
            let x, y = graphics_scale (x, y) in
              Graphics.moveto x y;
              Graphics.draw_string text;
              ""
end
