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
    "strid - A string diagrams generator\nstrid [options] file";
  let sin =
    let fi = open_in !file_in in
    let flen = in_channel_length fi in
    let buf = String.create flen in
      Lang.info (Printf.sprintf "Read %d bytes." (input fi buf 0 flen));
      close_in fi;
      buf
  in
  let m = matrix_of_ir (Parser.matrix Lexer.token (Lexing.from_string sin)) in
  let pst = Lang.process_matrix m in
  let fo = open_out !file_out in
    output_string fo "\\documentclass{article}\n\\usepackage{pstricks}\n\\begin{document}\n";
    output_string fo "\\begin{pspicture}(0,0)(10,10)\n";
    output_string fo pst;
    output_string fo "\\end{pspicture}\n";
    output_string fo "\\end{document}\n";
    close_out fo
