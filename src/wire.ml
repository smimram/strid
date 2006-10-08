type reldir = float * float
type dir = float * float

let showpoints = ref false

type output_kind = Pstricks

class virtual wire =
object (self)
  method virtual draw : output_kind -> string

  val mutable dependencies = ([]:wire list)

  (* Specify that another wire should be drawn before. *)
  method add_dep d =
    dependencies <- d::dependencies

  method get_deps = dependencies
end

class line (src:dir) (dst:dir) =
object (self)
  inherit wire

  method draw _ = failwith "Lines can't be drawn."

  val mutable src = src

  val mutable dst = dst

  val mutable attrs = ([]:(string * string) list)

  method add_attr name value =
    attrs <- (name, value)::attrs

  method get_attrs = attrs

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

  method draw outkind =
    (* let pls = self#split_attrs in
      Printf.printf "[DD] Split len: %d\n%!" (List.length pls);
      if List.length pls = 1 then
     ( (* The polyline is uniform. *) *)
    if List.length lines = 1 then
      (
        let xs, ys = (List.hd lines)#src in
        let xe, ye = (List.hd lines)#dst in
          Printf.sprintf "\\psline%s(%.2f,%.2f)(%.2f,%.2f)" (sp ()) xs ys xe ye
      )
    else
      (
        let x, y = (List.hd lines)#src in
        let s = ref (Printf.sprintf "\\%s%s(%.2f,%.2f)" (if self#src = self#dst then "psccurve" else "pscurve") (sp ()) x y) in
        let coords = List.map (fun l -> l#dst) lines in
        let coords =
          (* Remove duplicate coordinates. *)
          let rec uniq lx ly = function
            | (x,y)::t when x = lx && y = ly -> uniq lx ly t
            | (x,y)::t -> (x,y)::(uniq x y t)
            | [] -> []
          in
            uniq x y coords
        in
          List.iter (fun (x, y) -> s := !s ^ Printf.sprintf "(%.2f,%.2f)" x y) coords;
          !s
      )
      (* )
      else
        "\\pscustom{" ^ (List.fold_left (fun s pl -> s ^ pl#draw outkind ^ "\n") "" pls) ^ "}\n" *)
end

class ellipse pos r =
object (self)
  inherit wire

  val position = pos

  val radius = r

  method draw _ =
    let x, y = position in
    let xr, yr = radius in
      Printf.sprintf "\\psellipse[fillstyle=solid](%.2f,%.2f)(%.2f,%.2f)" x y xr yr
end

class text pos t =
object (self)
  inherit wire

  val position = pos

  val text = t

  method draw _ =
    let x, y = position in
      Printf.sprintf "\\rput(%.2f,%.2f){%s}" x y text
end
