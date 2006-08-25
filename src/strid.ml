let file_in = ref ""
let file_out = ref "out.tex"

let _ =
  Arg.parse
    [
      "-o", Arg.Set_string file_out, "Output file"
    ]
    (fun s ->
       file_in := s
    )
    "strid [options] file"
