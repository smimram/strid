let debug = Printf.printf "[DD] %s\n"
let info = Printf.printf "[II] %s\n"
let warning = Printf.printf "[WW] %s\n"

let iffound f =
  try f () with Not_found -> ()

let rd_add (x, y) (dx, dy) = (x +. dx, y +. dy)
type opt = string * ((string * string) list)

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
        | "line" ->
            let l = new Wire.polyline (new Wire.line c.(0) pos) in
              l#append_line (new Wire.line pos c.(1));
              l::[]
        | k ->
            warning (Printf.sprintf "Don't know lines for %s box." k); []

  method get_ellipses pos =
    let ans = ref [] in
      iffound (fun () -> let _ = List.assoc "l" options in ans := (new Wire.ellipse pos (0.5,0.3))::!ans);
      !ans

  method get_texts pos =
    let ans = ref [] in
      iffound
        (fun () ->
           let label = List.assoc "l" options in
           let t = List.assoc "t" label in
             ans := (new Wire.text pos t)::!ans
        );
      !ans
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
