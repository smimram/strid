(** A relative position. *)
type reldir = float * float

(** A position. *)
type dir = float * float

(** Show the points of polylines (for debugging). *)
val showpoints : bool ref

(** Kind of output. *)
type output_kind =
  | Pstricks (** pstricks output *)
  | Tikz (** tikz output *)
  | Graphics (** graphics output *)

val graphics_scale : float * float -> int * int

val compl_arrow : float -> float

(** Base class for geometrical figures. *)
class virtual wire :
object
  (** [add_attr attr val] sets the attibute [attr] to the value [val]. *)
  method add_attr : string -> string -> unit

  method add_attr_float : string -> float -> unit

  method has_attr : string -> bool

  method del_attr : string -> unit

  (** Get the value of an attribute. *)
  method get_attr : string -> string

  method get_attrs : string -> string list

  method get_attr_float : string -> float

  method get_attrs_float : string -> float list

  (** Get the value of an attribute returning a default value if the attribute
    * was not set. *)
  method get_attr_d : string -> string -> string

  (** Add a dependency (a [wire] that should be drawn before this one). *)
  method add_dep : wire -> unit

  (** Get all the dependencies. *)
  method get_deps : wire list

  (** Render the wire. *)
  method virtual draw : output_kind -> string
end

(** A line. *)
class line : dir -> dir ->
object
  inherit wire

  (** Source. *)
  method src : dir

  (** Target. *)
  method dst : dir

  method length : float

  (** Reverse the line. *)
  method rev : unit

  method draw : output_kind -> string

  method draw_arrow : output_kind -> string
end

(** A sequence of joined lines. *)
class polyline : line ->
object ('a)
  inherit wire

  (** Source. *)
  method src : dir

  (** Target. *)
  method dst : dir

  method length : float

  (** Lines this polyline is made of. *)
  method lines : line list

  method prepend : polyline -> unit
  method prepend_line : line -> unit
  method append : polyline -> unit
  method append_line : line -> unit

  (** Get the reversed polyline. *)
  method rev : 'a

  method draw : output_kind -> string
end

val new_polyline : dir list -> polyline

(** An ellipse. *)
class ellipse : dir -> dir ->
object
  inherit wire
  method draw : output_kind -> string
end

(** A rectangle. *)
class rectangle : dir -> dir ->
object
  inherit wire
  method draw : output_kind -> string
end

(** A polygon. *)
class polygon : dir list ->
object
  inherit wire
  method draw : output_kind -> string
end

(** Text. *)
class text : dir -> string ->
object
  inherit wire
  method draw : output_kind -> string
end
