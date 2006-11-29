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

type reldir = float * float
type dir = float * float

let showpoints = ref false

type output_kind = Pstricks | Tikz

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

  method get_attr name =
    List.assoc name attrs

  method get_attrs = attrs
end

class line (src:dir) (dst:dir) =
object (self)
  inherit wire

  method draw _ = failwith "Lines can't be drawn."

  val mutable src = src

  val mutable dst = dst

  method src = src

  method dst = dst

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
    lines <- List.rev lines;
    List.iter (fun l -> l#rev) lines;
    self

  method append_line line =
    lines <- lines@[line]

  method prepend_line line =
    lines <- line::lines

  (** Append a polyline at the end. *)
  method append (pl:polyline) =
    assert (self#dst = pl#src);
    List.iter self#append_line pl#lines

  (** Put a polyline before. *)
  method prepend (pl:polyline) =
    assert (self#src = pl#dst);
    List.iter self#prepend_line (List.rev pl#lines)

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

  (** Get the code for drawing the polyline. *)
  method draw outkind =
    match outkind with
      | Tikz
      | Pstricks ->
          (
            let resolution = ref 20 in (* number of generated points between two lines *)
              (* Remove trivial lines. *)
            let lines = List.rev (List.fold_left (fun ans l -> if l#src = l#dst then ans else (l::ans)) [] lines) in
              if lines = [] then "" else
                let points = (List.hd lines)#src::(List.map (fun l -> l#dst) lines) in
                  (* let points = remove_consecutive_dups points in *)
                  match points with
                    | (x1,y1)::(x2,y2)::[] ->
                        (match outkind with
                           | Tikz ->
                               Printf.sprintf "\\draw (%.2f,%.2f) -- (%.2f,%.2f);" x1 y1 x2 y2
                           | _ ->
                               Printf.sprintf "\\psline%s(%.2f,%.2f)(%.2f,%.2f)" (sp ()) x1 y1 x2 y2
                        )
                    | _::[] | [] -> failwith "Drawing empty line."
                    | points ->
                        let spl = Spline.compute ~periodic:(List.hd points = list_last points) !resolution points in
                        let spl = List.map snd spl in
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
                                     | _ ->
                                         Printf.sprintf "\\psline%s(%.2f,%.2f)" (sp ()) (fst !plast) (snd !plast)
                                  );
                                  for i = 0 to !resolution - 1 do
                                    plast := Queue.pop spl;
                                    ans := !ans ^
                                    (match outkind with
                                       | Tikz ->
                                           Printf.sprintf " -- (%.2f,%.2f)" (fst !plast) (snd !plast)
                                       | _ ->
                                           Printf.sprintf "(%.2f,%.2f)" (fst !plast) (snd !plast)
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
                      !ans
          )
end

(** Create a new polyline, given a list of points. *)
let new_polyline l =
  let rec aux pl = function
    | p::q::t ->
        pl#append_line (new line p q);
        aux pl (q::t);
        pl
    | _ -> pl
  in
    match l with
      | p::q::t -> aux (new polyline (new line p q)) (q::t)
      | _ -> failwith "Trying to create an empty polyline."

class ellipse pos r =
object (self)
  inherit wire

  val position = pos

  val radius = r

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
              Printf.sprintf "\\filldraw[fill=white%s] (%.2f,%.2f) ellipse (%.2fcm and %.2fcm);" bw x y xr yr
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
              Printf.sprintf "\\psellipse[fillstyle=solid%s](%.2f,%.2f)(%.2f,%.2f)" bw x y xr yr
end

class rectangle p1 p2 =
object (self)
  inherit wire

  val corner1 = p1

  val corner2 = p2

  method draw outkind =
    let x1, y1 = corner1 in
    let x2, y2 = corner2 in
      match outkind with
        | Tikz ->
            Printf.sprintf "\\draw[dashed] (%.2f,%.2f) rectangle (%.2f,%.2f);" x1 y1 x2 y2
        | Pstricks -> assert (false)
end

class text pos t =
object (self)
  inherit wire

  val position = pos

  val text = t

  method draw outkind =
    let x, y = position in
      match outkind with
        | Tikz ->
            Printf.sprintf "\\draw (%.2f,%.2f) node{%s};" x y text
        | Pstricks ->
            Printf.sprintf "\\rput(%.2f,%.2f){%s}" x y text
end
