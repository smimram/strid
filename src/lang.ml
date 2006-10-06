let debug = Printf.printf "[DD] %s\n"
let info = Printf.printf "[II] %s\n"
let warning = Printf.printf "[WW] %s\n"

let rd_add (x, y) (dx, dy) = (x+dx, y+dy)

class box (kind:string) (connexions:Wire.reldir list) =
object (self)
  method kind = kind

  method connexion = Array.of_list connexions

  method get_plines (pos : Wire.dir) =
    match self#kind with
      | "mult" ->
          let a = rd_add pos self#connexion.(0) in
          let b = rd_add pos self#connexion.(1) in
          let c = rd_add pos self#connexion.(2) in
          let i = new Wire.polyline (new Wire.line pos c) in
          let u = new Wire.polyline (new Wire.line a pos) in
            u#append (new Wire.polyline (new Wire.line pos b));
            i::u::[]
      | "arc" ->
          let a = rd_add pos self#connexion.(0) in
          let b = rd_add pos self#connexion.(1) in
          let u = new Wire.polyline (new Wire.line a pos) in
            u#append (new Wire.polyline (new Wire.line pos b));
            [u]
      | "sym" ->
          let c = Array.map (rd_add pos) self#connexion in
          let p = new Wire.polyline (new Wire.line c.(0) pos) in
          let q = new Wire.polyline (new Wire.line c.(1) pos) in
            p#append (new Wire.polyline (new Wire.line pos c.(3)));
            q#append (new Wire.polyline (new Wire.line pos c.(2)));
            p::q::[]
      | k ->
          warning (Printf.sprintf "Don't know lines for %s box." k); []
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

let rec reldir_of_dir = function
  | [] -> (0, 0)
  | d::t ->
      let (x, y) = reldir_of_dir t in
        match d with
          | Left -> (x-1, y)
          | Right -> (x+1, y)
          | Up -> (x, y+1)
          | Down -> (x, y-1)

let reldir_of_string s = reldir_of_dir (dirs_of_string s)

let matrix_of_ir ir =
  (* TODO: same length for every line *)
  Array.map (Array.of_list) (Array.of_list ir)

let rec join_plines plines =
  let rec find cur = function
    | [] -> []
    | h::t when cur#dst = h#src ->
        cur#append h;
        find cur t
    | h::t when cur#src = h#dst ->
        let a, b = cur#src in
        let c, d = h#dst in
        Printf.printf "src : %d, %d      dst : %d, %d\n%!" a b c d;
        cur#prepend h;
        find cur t
    | h::t when cur#dst = h#dst ->
        h#rev;
        cur#append h;
        find cur t
    | h::t when cur#src = h#src ->
        h#rev;
        cur#prepend h;
        find cur t
    | h::t ->
        let a, b = cur#dst in
        let c, d = cur#src in
          Printf.printf "dst : %d, %d      src : %d, %d\n%!" a b c d;
          h::(find cur t)
  in
    match plines with
      | [] -> []
      | h::t -> let tl = find h (join_plines t) in h::tl

let process_matrix m =
  let out = ref "" in
  let plines = ref [] in
  let add_box pos b =
    match b with
      | None -> ()
      | Some b ->
          debug (Printf.sprintf "New %s box." b#kind);
          plines := !plines@b#get_plines pos
  in
  let height = Array.length m - 1 in
    for i = 0 to height do
      for j = 0 to Array.length m.(i) - 1 do
        add_box (j, height - i) m.(i).(j)
      done
    done;
    plines := join_plines !plines;
    List.iter (fun pl -> out := !out ^ Printf.sprintf "%s\n" (pl#draw Wire.Pstricks)) !plines;
    !out
