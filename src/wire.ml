type reldir = int * int
type dir = int * int

type output_kind = Pstricks

class virtual wire =
object (self)
  method virtual draw : output_kind -> string
end

class line (src:dir) (dst:dir) =
object (self)
  val mutable src = src

  val mutable dst = dst

  method src = src

  method dst = dst

  method rev =
    let tmp = src in
      src <- dst;
      dst <- tmp
end

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

  method append (pl:polyline) =
    assert (self#dst = pl#src);
    List.iter self#append_line pl#lines

  method prepend (pl:polyline) =
    assert (self#src = pl#dst);
    List.iter self#prepend_line (List.rev pl#lines)

  method draw _ =
    if List.length lines = 1 then
      (
        let xs, ys = (List.hd lines)#src in
        let xe, ye = (List.hd lines)#dst in
          Printf.sprintf "\\psline[showpoints=true](%d,%d)(%d,%d)" xs ys xe ye
      )
    else
      (
        let fl = (List.hd lines)#src in
        let s = ref (Printf.sprintf "\\%s[showpoints=true](%d,%d)" (if self#src = self#dst then "psccurve" else "pscurve") (fst fl) (snd fl)) in
          List.iter (fun l -> let x, y = l#dst in s := !s ^ Printf.sprintf "(%d,%d)" x y) lines;
          !s
      )
end
