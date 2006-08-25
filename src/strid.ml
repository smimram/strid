open Lang

let file_in = ref ""
let file_out = ref "out.tex"

let get_pos d i j =
  (i*10, j*10)

let _ =
  Arg.parse
    [
      "-o", Arg.Set_string file_out, "Output file"
    ]
    (fun s ->
       file_in := s
    )
    "strid [options] file";
  let sin =
    let f = open_in !file_in in
    let buf = String.create (in_channel_length f) in
      Printf.printf "[II] Read %d bytes.\n" (input f buf 0 (in_channel_length f));
      close_in f;
      buf
  in
  let m = matrix_of_ir (Parser.matrix Lexer.token (Lexing.from_string sin)) in
  let d =
    {
      diag_matrix = m;
    }
  in
    ()
