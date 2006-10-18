open Common

let debug = Printf.printf "[DD] %s\n"
let info = Printf.printf "[II] %s\n"
let warning = Printf.printf "[WW] %s\n"
let error e = Printf.printf "[EE] %s\n" e; exit 1

let ellipse_X_ray = ref 0.5
let ellipse_Y_ray = ref 0.3
let circle_ray = ref 0.3
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
    (px +. dx/.norm *. !circle_ray , py +. dy/.norm *. !circle_ray)

class box (kind:string) (connexions:Wire.reldir list) (options:opt list) =
object (self)
  val options = options

  method get_attr name subname =
    List.assoc subname (List.assoc name options)

  method kind = kind

  method connexion = Array.of_list connexions

  method get_plines pos =
    let c = Array.map (rd_add pos) self#connexion in
      match self#kind with
        | "mult" ->
            let i = Wire.new_polyline (pos::(*(circle_position pos c.(2))::*)c.(2)::[]) in
            let u = Wire.new_polyline (c.(0)::pos::c.(1)::[]) in
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
      | _ ->
          deffound []
            (fun () ->
               let _ = List.assoc "l" options in (* label *)
               let xray = deffound !ellipse_X_ray (fun () -> float_of_string (self#get_attr "l" "w")) in
               let yray = deffound !ellipse_Y_ray (fun () -> float_of_string (self#get_attr "l" "h")) in
               let e = new Wire.ellipse pos (xray, yray) in
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

let process_matrix kind m =
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
       | Wire.Tikz -> "\\begin{tikzpicture}\n"
       | _ -> Printf.sprintf "\\begin{pspicture}(0,0)(%d,%d)\n" (!width + 1) height
    );
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s\n" (pl#draw kind)) !plines;
    List.iter (fun e -> out := !out ^ Printf.sprintf "%s\n" (e#draw kind)) !ellipses;
    List.iter (fun t -> out := !out ^ Printf.sprintf "%s\n" (t#draw kind)) !texts;
    out := !out ^
    (match kind with
       | Wire.Tikz ->
       "\\end{tikzpicture}\n"
       | _ ->
           "\\end{pspicture}\n"
    );
    !out
