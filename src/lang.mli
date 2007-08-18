type environment = (string * string) list

type opt = string * (string * string) list

val reldir_of_string : string -> float * float

class box : string -> Wire.reldir list -> opt list ->
object
  method set_env : environment -> unit
  method get_label_decorations : float * float -> Wire.rectangle list
  method get_plines : Wire.dir -> Wire.polyline list
  method get_texts : float * float -> Wire.text list
  method kind : string
end

type line = box option list
type ir_matrix = line list
type matrix = box option array array
type dir = Left | Right | Up | Down
val matrix_of_ir : environment -> box option list list -> box option array array
val process_matrix : Wire.output_kind -> environment -> box option array array -> string
