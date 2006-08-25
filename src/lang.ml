type reldir = int * int
type box =
    {
      box_name : string;
      box_dir : reldir;
      box_connexions : reldir list;
    }
type line = box option list
type ir_matrix = line list
type matrix = box option array array
type diagram =
    {
      diag_matrix : matrix;
    }
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
          | Up -> (x, y-1)
          | Down -> (x, y+1)

let reldir_of_string s = reldir_of_dir (dirs_of_string s)

let matrix_of_ir ir =
  (* TODO: same length for every line *)
  Array.map (Array.of_list) (Array.of_list ir)
