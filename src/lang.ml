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

let marie_number = (sqrt 5. -. 1.) /. 2.

let iffound f =
  try f () with Not_found -> ()

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
let re_square = Str.regexp "square\\(\\([0-9]*[uldr]\\)*\\)"
let dir_of_square s =
  let ans = ref [] in
  let n = ref 0 in
    for i = 0 to String.length s - 1 do
      match s.[i] with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
            n := 10 * !n + int_of_char s.[i] - int_of_char '0'
        | 'u' | 'r' | 'd' | 'l' ->
            let m = if !n = 0 then 1 else !n in
              for _ = 0 to m - 1 do
                ans := s.[i] :: !ans
              done;
              n := 0
        | _ -> assert false
    done;
    List.rev !ans
let dir_of_square k =
  assert (Str.string_match re_square k 0);
  dir_of_square (Str.matched_group 1 k)
(*
let ndir_of_square l =
  let ans = Array.make 4 0 in
    List.iter
      (function
         | 'u' ->
             ans.(0) <- ans.(0) + 1
         | 'r' ->
             ans.(1) <- ans.(1) + 1
         | 'd' ->
             ans.(2) <- ans.(2) + 1
         | 'l' ->
             ans.(3) <- ans.(3) + 1
         | _ -> assert false
      ) l
*)

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
  let coeff =  marie_number in
  let r = height *. coeff in
  let r' = height *. (1. -. coeff) in
  let up = px +. r *. dx, py +. r *. dy in
  let left = px -. r' *. dx +. width /. 2. *. ox, py -. r' *. dy +. width /. 2. *. oy in
  let right = px -. r' *. dx -. width /. 2. *. ox, py -. r' *. dy -. width /. 2. *. oy in
    [up; left; right; up]

(*
let middle p q =
  let xs, ys = p in
  let xt, yt = q in
    (xt -. xs) /. 2. , (yt -. ys) /. 2.
*)

class box (kind:string) (connections:Wire.reldir list) (options:opt list) =
object (self)
  method private get_attr name ?d subname =
    try
      List.assoc subname (List.assoc name options)
    with
      | Not_found ->
          (
            match d with
              | Some d -> d
              | None -> raise Not_found
          )

  method private get_arrows =
    let arrows = may_map (fun (n, o) -> if n = "a" then Some o else None) options in
    let ans = ref [] in
      List.iter
        (fun o ->
           let dir = may_assoc "f" "d" o in
           let dir = if dir = "f" then 1. else if dir = "b" then -1. else error "Direction of arrows should be either 'f' or 'b'." in
           let t = dir *. (float_of_string (may_assoc "0.5" "t" o)) in
             ans := t :: !ans
        ) arrows;
      !ans

  method private get_attr_float name ?d subname =
    (* TODO: don't use strings for floats *)
    float_of_string (self#get_attr name ?d:(map_some string_of_float d) subname)

  method private has_attr name =
    try
      ignore (List.assoc name options); true
    with
      | Not_found -> false

  method private has_subattr name subname =
    try
      ignore (self#get_attr name subname);
      true
    with
      | Not_found -> false

  val mutable kind = kind

  method kind = kind

  method set_kind k = kind <- k

  val mutable connections = Array.of_list connections

  method connections = connections

  method set_connections c = connections <- c

  method get_plines pos =
    let c = Array.map (Vect.add pos) self#connections in
      match self#kind with
        | "mult" ->
            let i = Wire.new_polyline [pos; c.(2)] in
            let u = Wire.new_polyline [c.(0); ortho_point pos c.(2) c.(0) c.(1); pos] in
            let u' = Wire.new_polyline [c.(1); ortho_point pos c.(2) c.(1) c.(0); pos] in
              List.iter
                (fun t ->
                   u#add_attr_float "a" t;
                   u'#add_attr_float "a" t;
                   i#add_attr_float "a" (Wire.compl_arrow t)
                ) self#get_arrows;
              u#append (u'#rev);
              [i; u]
        | "arc" ->
            let u = new Wire.polyline (new Wire.line c.(0) pos) in
              u#append_line (new Wire.line pos c.(1));
              [u]
        | "sym" ->
            let p = Wire.new_polyline [c.(0); pos] in
            let p' = Wire.new_polyline [pos; c.(3)] in
            let q = Wire.new_polyline [c.(1); pos] in
            let q' = Wire.new_polyline [pos; c.(2)] in
              List.iter
                (fun t ->
                   p#add_attr_float "a" t;
                   p'#add_attr_float "a" (Wire.compl_arrow t);
                   q#add_attr_float "a" t;
                   q'#add_attr_float "a" (Wire.compl_arrow t);
                ) self#get_arrows;
              p#append p';
              q#append q';
              [p; q]
        | "braid" ->
            let p1 = Wire.new_polyline [c.(1); circle_position pos c.(1)] in
            let l = new Wire.line (circle_position pos c.(1)) (circle_position pos c.(2)) in
            let p2 = Wire.new_polyline [circle_position pos c.(2); c.(2)] in
              (* let q = Wire.new_polyline [c.(0); pos; c.(3)] in *)
            let q =
              if Conf.get_bool "drive_braids" then
                Wire.new_polyline [c.(0); circle_position pos c.(0); circle_position pos c.(3); c.(3)]
              else
                Wire.new_polyline [c.(0); pos; c.(3)]
            in
              l#add_attr "opacity" "0";
              p1#append_line l;
              p1#append p2;
              [p1; q]
        | "line"
        | "curve" as k ->
            let l =
              (if k = "line" then Wire.new_polyline else Wire.new_curve)
              (if Array.length c >= 2 then
                 [c.(0); pos]@(List.tl (Array.to_list c))
               else
                 [pos; c.(0)])
            in
              List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
              iffound (fun () -> l#add_attr_float "width" (self#get_attr_float "w" "w")); (* wire width *)
              iffound (fun () -> l#add_attr "color" (self#get_attr "w" "c")); (* color *)
              iffound (fun () -> l#add_attr "style" (self#get_attr "w" "s")); (* style *)
              [l]
        | "antipode" ->
            if Array.length c <= 1 then
              []
            else
              let l1 = Wire.new_polyline [c.(0); pos] in
              let l2 = Wire.new_polyline [pos; c.(1)] in
                List.iter (fun t -> l1#add_attr_float "a" t; l2#add_attr_float "a" t) self#get_arrows;
                l1#append l2;
                [l1]
        | "adj" ->
            let l = Wire.new_polyline [c.(0); pos; c.(1)] in
              List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
              [l]
        | "unit" ->
            let l = Wire.new_polyline [pos; c.(0)] in
              List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
              [l]
        | "text" -> []
        | "region" -> []
        | "vbox" ->
            let px, _ = pos in
            let u =
              Array.map
                (fun (x,y) ->
                   let l = Wire.new_polyline [x,y; px+.(x+.2.)*.epsilon,y] in
                     List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
                     l
                ) c
            in
              Array.to_list u
        | "hbox" ->
            let _, py = pos in
            let u =
              Array.map
                (fun (x,y) ->
                   let l = Wire.new_polyline [x,y; x,py+.(y+.2.)*.epsilon] in
                     List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
                     l
                ) c
            in
              Array.to_list u
        | "operad" ->
            let i = Array.length c - 1 in
            let ans = ref [] in
            let height = self#get_attr_float "l" ~d:(Conf.get_float "label_triangle_height") "h" in
            let width = self#get_attr_float "l" ~d:(Conf.get_float "label_triangle_height" *. 2. /. sqrt 3.) "w" in
            let dir = Vect.normalize (Vect.sub c.(i) pos) in
            let tp = triangle_points pos dir height width in
            let right_x, right_y = List.hd (List.tl tp) in
            let left_x, left_y = List.hd (List.tl (List.tl tp)) in
              for n = 0 to i - 1 do
                let pl =
                  Wire.new_polyline
                    [
                      c.(n);
                      left_x +. (float_of_int n +. 1.) /. (float_of_int i +. 1.) *. (right_x -. left_x),
                      left_y +. (float_of_int n +. 1.) /. (float_of_int i +. 1.) *. (right_y -. left_y)
                    ]
                in
                  ans := pl::!ans
              done;
              let pl = Wire.new_polyline [List.hd tp; c.(i)] in
                ans := pl::!ans;
                !ans
        | k when Str.string_match re_box k 0 ->
            let i = int_of_string (Str.matched_group 1 k) in
            let o = int_of_string (Str.matched_group 2 k) in
            let px, py = pos in
            let ans = ref [] in
              for n = 0 to i - 1 do
                if c.(n) <> pos then
                  let pl = Wire.new_polyline [c.(n); (*circle_position pos c.(n);*) px +. (float_of_int (n + 1)) *. epsilon, py]
                  in
                    List.iter (fun t -> pl#add_attr_float "a" t) self#get_arrows;
                    ans := pl::!ans
              done;
              for n = i to i + o - 1 do
                if c.(n) <> pos then
                  let pl = Wire.new_polyline [px +. (float_of_int (n + 1)) *. epsilon, py; (*circle_position pos c.(n);*) c.(n)]
                  in
                    List.iter (fun t -> pl#add_attr_float "a" t) self#get_arrows;
                    ans := pl::!ans
              done;
              !ans
        | k when Str.string_match re_square k 0 ->
            let px, py = pos in
            let dir = dir_of_square k in
            let ans = ref [] in
            let e = ref 0 in (* epsilon perturbation *)
              List.iter2
                (fun (x,y) dir ->
                   incr e;
                   let x', y' =
                     match dir with
                       | 'u' | 'd' ->
                           x, py +. (float !e) *. epsilon
                       | 'l' | 'r' ->
                           px +. (float !e) *. epsilon, y
                       | _ -> assert false
                   in
                   let l = Wire.new_polyline [x, y; x', y'] in
                     List.iter (fun t -> l#add_attr_float "a" t) self#get_arrows;
                     ans := l :: !ans
                ) (Array.to_list c) dir;
              !ans
        | k ->
            error (Printf.sprintf "Don't know lines for %s box." k)

  method get_label_decorations pos =
    let c = Array.map (Vect.add pos) self#connections in
      match self#kind with
        | "region" ->
            let p0, p1 = if Array.length c >= 2 then c.(0), c.(1) else pos, c.(0) in
            let r = new Wire.rectangle p0 p1 in
              r#add_attr "style" (self#get_attr "l" ~d:"dashed" "b"); (* border line *)
              [r]
        | "antipode" ->
            let e = new Wire.ellipse pos (0.14, 0.14) in
              (
                try
                  e#add_attr "color" (self#get_attr "l" "c")
                with
                  | Not_found -> e#add_attr "color" (Conf.get "antipode_color")
              );
              [e]
        | "unit" when not (self#has_subattr "l" "t") ->
            let e = new Wire.ellipse pos (0.14, 0.14) in
              iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
              [e]
        | "vbox" ->
            let dx = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_width") "w") /. 2. in
            let dy = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_height") "h") /. 2. in
            let px, py = pos in
            let y, y' = Array.fold_left (fun (y,y') (_,cy) -> min y cy, max y' cy) (py,py) c in
            let r = new Wire.rectangle (px-.dx,y-.dy) (px+.dx,y'+.dy) in
              r#add_attr "color" (self#get_attr "l" ~d:"white" "s");
              [r]
        | "hbox" ->
            let dx = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_width") "w") /. 2. in
            let dy = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_height") "h") /. 2. in
            let px, py = pos in
            let x, x' = Array.fold_left (fun (x,x') (cx,_) -> min x cx, max x' cx) (px,px) c in
            let r = new Wire.rectangle (x-.dx,py-.dy) (x'+.dx,py+.dy) in
              r#add_attr "color" (self#get_attr "l" ~d:"white" "s");
              [r]
        | k when Str.string_match re_square k 0 ->
            let dx = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_width") "w") /. 2. in
            let dy = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_height") "h") /. 2. in
            let px, py = pos in
            let dir = dir_of_square k in
            let v = ref (py,py) in
            let h = ref (px,px) in
              List.iter2
                (fun (x,y) dir ->
                   match dir with
                     | 'd' | 'u' ->
                         let hl, hr = !h in
                           h := (min x hl, max x hr)
                     | 'l' | 'r' ->
                         let vd, vu = !v in
                           v := (min y vd, max y vu)
                     | _ -> assert false
                ) (Array.to_list c) dir;
              let vd, vu = !v in
              let hl, hr = !h in
              let r = new Wire.rectangle (hl-.dx,vd-.dy) (hr+.dx,vu+.dy) in
                r#add_attr "color" (self#get_attr "l" ~d:"white" "s");
                [r]
        | _ ->
            deffound []
              (fun () ->
                 let () =
                   if self#kind <> "operad" then
                     (* Do we have a label? *)
                     ignore (List.assoc "l" options)
                 in
                 let shape =
                   deffound
                     (if self#kind = "operad" then
                        (* The default shape for operads is a triangle. *)
                        "triangle"
                      else if self#kind = "text" then
                        "none"
                      else
                        "ellipse")
                     (fun () -> self#get_attr "l" "s")
                 in
                 let pos =
                   if self#kind = "text" then
                     (
                       let px, py = pos in
                       let pxt, pyt =
                         if Array.length c >= 1 then c.(0) else 0., 0.
                       in
                       let p = (px +. pxt) /. 2., (py +. pyt) /. 2. in
                         p
                     )
                   else
                     pos
                 in
                   match shape with
                     | "r"
                     | "rectangle" ->
                         let dx = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_width") "w") /. 2. in
                         let dy = (self#get_attr_float "l" ~d:(Conf.get_float "label_rectangle_height") "h") /. 2. in
                         let x,y = pos in
                         let e = new Wire.polygon [x-.dx,y-.dy; x-.dx,y+.dy; x+.dx,y+.dy; x+.dx,y-.dy; x-.dx,y-.dy] in
                           iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
                           [e]
                     | "t"
                     | "triangle" ->
                         let height = self#get_attr_float "l" ~d:(Conf.get_float "label_triangle_height") "h" in
                         let width =self#get_attr_float "l" ~d:(Conf.get_float "label_triangle_height" *. 2. /. sqrt 3.) "w" in
                         (* direction *)
                         let dir =
                           deffound
                             (if self#kind = "operad" then
                                (* We can guess the direction for operads. *)
                                Vect.normalize (Vect.sub c.(Array.length c - 1) pos)
                              else
                                Vect.normalize (reldir_of_string "u"))
                             (fun () -> Vect.normalize (reldir_of_string (self#get_attr "l" "d")))
                         in
                         let e = new Wire.polygon (triangle_points pos dir height width) in
                           iffound (fun () -> e#add_attr "color" (self#get_attr "l" "c"));
                           [e]
                     | "e"
                     | "ellipse" ->
                         let xray = self#get_attr_float "l" ~d:(Conf.get_float "label_width") "w" in
                         let yray = self#get_attr_float "l" ~d:(Conf.get_float "label_height") "h" in
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
    let c = Array.map (Vect.add pos) self#connections in
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

exception Invalid_box of string

let make_box kind connections options =
  let arity = List.length connections in
  let arity_ok =
    match kind with
      | "mult" -> arity = 3
      | "arc" -> arity = 2
      | "sym" -> arity = 4
      | "braid" -> arity = 4
      | "line" -> true (* arity <= 2 *)
      | "curve" -> true
      | "antipode" -> arity <= 2
      | "adj" -> arity = 2
      | "unit" -> arity = 1
      | "text" -> arity = 1
      | "vbox" | "hbox" -> true
      | "region" -> arity <= 2
      | "operad" -> arity >= 1
      | k when Str.string_match re_box k 0 ->
          let i = int_of_string (Str.matched_group 1 k) in
          let o = int_of_string (Str.matched_group 2 k) in
            arity = i + o
      | k when Str.string_match re_square k 0 ->
          arity = List.length (dir_of_square k)
      | _ -> raise (Invalid_box (Printf.sprintf "%s is not a valid box type." kind))
  in
    if not arity_ok then
      raise (Invalid_box (Printf.sprintf "%s boxes cannot have %d arguments." kind arity));
    new box kind connections options

type line = box list list
type ir_matrix =
    {
      ir_options : string list;
      ir_lines : line list;
    }
type matrix = box list array array
type dir = Left | Right | Up | Down

let matrix_of_ir ir =
  let matrix =
    Array.map (fun l -> Array.of_list l) (Array.of_list ir.ir_lines)
  in
  let matrix = ref matrix in
  let vmirror () =
    let height = Array.length !matrix in
      Array.iter
        (fun l ->
           Array.iter
             (fun b ->
                List.iter
                  (fun b ->
                     b#set_connections (Array.map (fun (x,y) -> x,-.y) b#connections)
                  ) b
             ) l
        ) !matrix;
      for i = 0 to (height - 1) / 2 do
        let tmp = !matrix.(i) in
          !matrix.(i) <- !matrix.(height - 1 - i);
          !matrix.(height - 1 - i) <- tmp
      done
  in
  let hmirror () =
    let height = Array.length !matrix in
    let width = Array.fold_left (fun n a -> max n (Array.length a)) 0 !matrix in
      Array.iter
        (fun l ->
           Array.iter
             (fun b ->
                List.iter
                  (fun b ->
                     b#set_connections (Array.map (fun (x,y) -> -.x,y) b#connections)
                  ) b
             ) l
        ) !matrix;
      for i = 0 to height - 1 do
        let m = !matrix.(i) in
        let m = Array.init width (fun j -> if j < Array.length m then m.(j) else []) in
          for j = 0 to (width - 1) / 2 do
            let tmp = m.(j) in
              m.(j) <- m.(width - 1 - j);
              m.(width - 1 - j) <- tmp
          done;
          !matrix.(i) <- m
      done
  in
  let rotate () =
    let height = Array.length !matrix in
    let width = Array.fold_left (fun n a -> max n (Array.length a)) 0 !matrix in
      Array.iter
        (fun l ->
           Array.iter
             (fun b ->
                List.iter
                  (fun b ->
                     if b#kind = "vbox" then
                       b#set_kind "hbox"
                     else if b#kind = "hbox" then
                       b#set_kind "vbox";
                     b#set_connections (Array.map (fun (x,y) -> y,-.x) b#connections)
                  ) b
             ) l
        ) !matrix;
      matrix :=
      Array.init width
        (fun i ->
           Array.init height
             (fun j ->
                let i, j = height-1-j, i in
                let m = !matrix.(i) in
                  if j < Array.length m then
                    m.(j)
                  else
                    []
             )
        )
  in
  let re_scale = Str.regexp "scale=\\([0-9\\.]*\\)" in
  let re_xscale = Str.regexp "xscale=\\([0-9\\.]*\\)" in
  let re_yscale = Str.regexp "yscale=\\([0-9\\.]*\\)" in
    List.iter
      (function
         | "vmirror" -> vmirror ()
         | "hmirror" -> hmirror ()
         | "rotate" -> rotate ()
         | "antirotate" -> rotate (); rotate (); rotate ()
         | m when Str.string_match re_scale m 0 ->
             let s = float_of_string (Str.matched_group 1 m) in
               Conf.set_float "scaling_factor" (s *. (Conf.get_float "scaling_factor"))
         | m when Str.string_match re_xscale m 0 ->
             let s = float_of_string (Str.matched_group 1 m) in
               Conf.set_float "xscale" (s *. (Conf.get_float "xscale"))
         | m when Str.string_match re_yscale m 0 ->
             let s = float_of_string (Str.matched_group 1 m) in
               Conf.set_float "yscale" (s *. (Conf.get_float "yscale"))
         | m -> error (Printf.sprintf "Unknown matrix modifier: %s." m)
      ) ir.ir_options;
    !matrix

let rec join_plines (plines:Wire.polyline list) =
  let eq = float_approx2 in
  let rec find cur = function
    | [] -> []
    | h::t when h#connects && eq cur#dst h#src ->
        cur#append h;
        find cur t
    | h::t when h#connects && eq cur#src h#dst ->
        cur#prepend h;
        find cur t
    | h::t when h#connects && eq cur#dst h#dst ->
        cur#append (h#rev);
        find cur t
    | h::t when h#connects && eq cur#src h#src ->
        cur#prepend (h#rev);
        find cur t
    | h::t ->
        h::(find cur t)
  in
    match plines with
      | [] -> []
      | h::t ->
          let tl =
            if h#connects then
              find h (join_plines t)
            else
              join_plines t
          in
            h::tl

let process_matrix kind m =
  let out = ref "" in
  let plines = ref [] in
  let ldeco = ref [] in
  let texts = ref [] in
  let add_box pos b =
    List.iter
      (fun b ->
         debug (Printf.sprintf "New %s box." b#kind);
         plines := !plines@b#get_plines pos;
         ldeco := !ldeco@b#get_label_decorations pos;
         texts := !texts@b#get_texts pos
      ) b
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
    if Conf.get_bool "show_grid" then
      (
        for i = 0 to !width do
          for j = 0 to height do
            let i = float i in
            let j = float j in
            let wh = Wire.new_polyline [i-.0.1,j; i+.0.1,j] in
            let wv = Wire.new_polyline [i,j-.0.1; i,j+.0.1] in
              wh#add_attr "color" "red";
              wv#add_attr "color" "red";
              plines := wh :: wv :: !plines
          done
        done
      );
    out :=
    (match kind with
       | Wire.Tikz ->
           let params =
             (if Conf.get_bool "center_vertically" then ["baseline=(current bounding box.center)"] else [])
             @(if (Conf.get_float "xscale") = 1. then [] else [Printf.sprintf "xscale=%.02f" (Conf.get_float "xscale")])
             @(if (Conf.get_float "yscale") = 1. then [] else [Printf.sprintf "yscale=%.02f" (Conf.get_float "yscale")])
             @(if (Conf.get_float "scaling_factor") = 1. then [] else [Printf.sprintf "scale=%.02f" (Conf.get_float "scaling_factor")])
             @(if (Conf.get_string "line_width") = "0.5pt" then [] else [Printf.sprintf "line width=%s" (Conf.get_string "line_width")])
           in
           let params = String.concat "," params in
           let params = if params = "," then "" else "[" ^ params ^ "]" in
           let params =
             let opts = Conf.get_string "tikz_options" in
             params ^ if opts = "" then "" else "," ^ opts
           in
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
           let w, h = float_of_int !width, float_of_int height in
           let w, h = max w 1., max h 1. in
           let w, h = Wire.graphics_scale (w, h) in
           Graphics.open_graph "";
           Graphics.resize_window (max w (Graphics.size_x ())) (max h (Graphics.size_y ()));
           Graphics.set_line_width 1;
           Graphics.set_color (Graphics.rgb 200 200 200);
           Graphics.draw_poly_line [|0, h; w, h; w, 0|];
           (* Graphics.draw_rect 0 0 w h; *)
           Graphics.set_color Graphics.black;
           Graphics.set_window_title "Strid";
           ""
    );
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s" (pl#draw kind)) !plines;
    List.iter (fun e -> out := !out ^ Printf.sprintf "%s" (e#draw kind)) !ldeco;
    List.iter (fun t -> out := !out ^ Printf.sprintf "%s" (t#draw kind)) !texts;
    out := !out ^
    (match kind with
       | Wire.Tikz ->
           if (Conf.get_bool "no_tex_environment") then "" else "\\end{tikzpicture}\n"
       | Wire.Pstricks -> "\\end{pspicture}\n"
       | Wire.Graphics -> ""
    );
    !out
