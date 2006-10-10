open Common

let debug = Printf.printf "[DD] %s\n"
let info = Printf.printf "[II] %s\n"
let warning = Printf.printf "[WW] %s\n"

let iffound f =
  try f () with Not_found -> ()

let rd_add (x, y) (dx, dy) = (x +. dx, y +. dy)
type opt = string * ((string * string) list)

let re_box = Str.regexp "\\([0-9]+\\)box\\([0-9]+\\)"

let get_dir (xs, ys) (xt, yt) =
  let dx = if xs = xt then 0. else (xt -. xs) /. (abs_float (xt -. xs)) in
  let dy = if ys = yt then 0. else (yt -. ys) /. (abs_float (yt -. ys)) in
    dx, dy

class box (kind:string) (connexions:Wire.reldir list) (options:opt list) =
object (self)
  val options = options

  method kind = kind

  method connexion = Array.of_list connexions

  method get_plines pos =
    let c = Array.map (rd_add pos) self#connexion in
      match self#kind with
        | "mult" ->
            let i = new Wire.polyline (new Wire.line pos c.(2)) in
            let u = new Wire.polyline (new Wire.line c.(0) pos) in
              u#append_line (new Wire.line pos c.(1));
              i::u::[]
        | "arc" ->
            let u = new Wire.polyline (new Wire.line c.(0) pos) in
              u#append_line (new Wire.line pos c.(1));
              [u]
        | "sym" ->
            let p = new Wire.polyline (new Wire.line c.(0) pos) in
            let q = new Wire.polyline (new Wire.line c.(1) pos) in
              p#append_line (new Wire.line pos c.(3));
              q#append_line (new Wire.line pos c.(2));
              p::q::[]
        | "braid" ->
            (* let pl1 = new Wire.line c.(0) pos in
            let pl2 = new Wire.line pos c.(3) in
            let ql1 = new Wire.line c.(1) pos in
            let ql2 = new Wire.line pos c.(2) in
            let p = new Wire.polyline pl1 in
            let q = new Wire.polyline ql1 in
              p#append_line pl2;
              q#append_line ql2;
              (* q should be over *)
              ql1#add_attr "vwidth" "yes"; ql2#add_attr "vwidth" "yes";
              List.iter (fun (ql, pl) -> ql#add_dep (pl :> Wire.wire)) [ql1,pl1;ql1,pl2;ql2,pl1;ql2,pl2];
              p::q::[] *)
            let pd = 0.2 in (* diff around the center *)
            let px, py = pos in
            let pdx, pdy = get_dir c.(0) c.(3) in
            (* let pdx = (pxt -. pxs) /. (abs_float (pxt -. pxs)) in
            let pdy = (pyt -. pys) /. (abs_float (pyt -. pys)) in *)
            let ppt = px -. pdx *. pd, py -. pdy *. pd in
            let pps = px +. pdx *. pd, py +. pdy *. pd in
            let p1 = new Wire.polyline (new Wire.line c.(0) ppt) in
            let p2 = new Wire.polyline (new Wire.line pps c.(3)) in
            let q = new Wire.polyline (new Wire.line c.(1) pos) in
              q#append_line (new Wire.line pos c.(2));
            (* let q = new Wire.polyline (new Wire.line c.(1) c.(2)) in *)
              p1::p2::q::[]
        | "line" ->
            let l = new Wire.polyline (new Wire.line c.(0) pos) in
              l#append_line (new Wire.line pos c.(1));
              l::[]
        | "text" -> []
        | k when Str.string_match re_box k 0 ->
            let i = int_of_string (Str.matched_group 1 k) in
            let o = int_of_string (Str.matched_group 2 k) in
            let px, py = pos in
            let ans = ref [] in
              for n = 0 to i - 1 do
                ans := (new Wire.polyline (new Wire.line c.(n) (px +. (float_of_int n) *. epsilon_float, py)))::!ans
              done;
              for n = i to i + o - 1 do
                ans := (new Wire.polyline (new Wire.line (px +. (float_of_int n) *. epsilon_float, py) c.(n)))::!ans
              done;
              !ans
        | k ->
            warning (Printf.sprintf "Don't know lines for %s box." k); []

  method get_ellipses pos =
    match self#kind with
      | "text" -> []
      | _ ->
          deffound []
            (fun () ->
               let _ = List.assoc "l" options in (* label *)
               let e = new Wire.ellipse pos (0.5,0.3) in
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
                 let label = List.assoc "l" options in (* label *)
                 let t = List.assoc "t" label in (* text *)
                   [new Wire.text pos t]
              )
end

type line = box option list
type ir_matrix = line list
type matrix = box option array array
type dir = Left | Right | Up | Down

let string_of_char = String.make 1

let dirs_of_string s =
  let ret = ref [] in
    for i = 0 to (String.length s) - 1
    do
      ret :=
      (
        match s.[i] with
          | 'l' -> Left
          | 'r' -> Right
          | 'u' -> Up
          | 'd' -> Down
          | _ -> failwith ("Invalid direction " ^ (string_of_char s.[i]))
      )::!ret
    done; !ret

let rec reldir_of_dir d =
  let rec aux = function
    | [] -> (0, 0)
    | d::t ->
        let (x, y) = aux t in
          match d with
            | Left -> (x-1, y)
            | Right -> (x+1, y)
            | Up -> (x, y+1)
            | Down -> (x, y-1)
  in
  let x, y = aux d in
    (float_of_int x), (float_of_int y)

let reldir_of_string s = reldir_of_dir (dirs_of_string s)

let matrix_of_ir ir =
  Array.map (Array.of_list) (Array.of_list ir)

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

let process_matrix m =
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
    out := (Printf.sprintf "\\begin{pspicture}(0,0)(%d,%d)\n" (!width + 1) height);
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s\n" (pl#draw Wire.Pstricks)) !plines;
    List.iter (fun e -> out := !out ^ Printf.sprintf "%s\n" (e#draw Wire.Pstricks)) !ellipses;
    List.iter (fun t -> out := !out ^ Printf.sprintf "%s\n" (t#draw Wire.Pstricks)) !texts;
    out := !out ^ "\\end{pspicture}\n";
    !out
