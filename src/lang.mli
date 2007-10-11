(** An option. *)
type opt = string * (string * string) list

(** Convert a string representing a relative direction into a relative
  * direction. *)
val reldir_of_string : string -> float * float

(** [box name dirs opts] creates a new object representing an operation of kind
  * [name] with [dirs] as relative positions for inputs and outputs and [opt] as
  * options. *)
class box : string -> Wire.reldir list -> opt list ->
object
  (** Get the decorations for the labels, given the position of the box. *)
  method get_label_decorations : float * float -> Wire.rectangle list

  (** Get the polylines for the strings of the box. *)
  method get_plines : Wire.dir -> Wire.polyline list

  (** Get the texts of the labels. *)
  method get_texts : float * float -> Wire.text list

  (** Get the kind of the box. *)
  method kind : string

  (** Get the connections. *)
  method connections : Wire.reldir array

  (** Set the connexions. *)
  method set_connections : Wire.reldir array -> unit
end

(** A relative direction. *)
type dir = Left | Right | Up | Down

(** A line of the matrix. *)
type line = box option list

(** Intermediate representation of the matrix. *)
type ir_matrix = line list

(** Abstract representation of a matrix. *)
type matrix = box option array array

(** Get a matrix from the intermediate representation. *)
val matrix_of_ir : ir_matrix -> matrix

(** Generate the image from a matrix. *)
val process_matrix : Wire.output_kind -> matrix -> string
