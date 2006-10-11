open Lang

let file_in = ref ""
let file_out = ref "strid.tex"
let full_tex = ref false

let get_pos d i j =
  (i*10, j*10)

let usage = "strid -- A string diagrams generator\nusage: strid [options] file"

let _ =
  Arg.parse
    [
      "--full-tex", Arg.Set full_tex, "Full LaTeX file";
      "-o", Arg.Set_string file_out, "Output file"
    ]
    (fun s ->
       file_in := s
    )
    usage;
  if !file_in = "" then
    (
      Printf.printf "%s\n%!" usage;
      exit 1
    );
  let sin =
    let fi = open_in !file_in in
    let flen = in_channel_length fi in
    let buf = String.create flen in
      Lang.debug (Printf.sprintf "Read %d bytes." (input fi buf 0 flen));
      close_in fi;
      buf
  in
  let m = matrix_of_ir (Parser.matrix Lexer.token (Lexing.from_string sin)) in
  let pst = Lang.process_matrix Wire.Pstricks m in
  let fo = open_out !file_out in
    if !full_tex then
      output_string fo "\\documentclass{article}\n\\usepackage{pstricks}\n\\begin{document}\n";
    output_string fo pst;
    if !full_tex then
      output_string fo "\\end{document}\n";
    close_out fo
