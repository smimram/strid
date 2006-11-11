open Lang

let re_file_in = Str.regexp "\\(.*\\)\\.strid"
let file_in = ref ""
let file_out = ref ""
let full_tex = ref false
let out_kind = ref "tikz"

let get_pos d i j =
  (i*10, j*10)

let usage = "strid -- A string diagrams generator\nusage: strid [options] file"

let _ =
  Arg.parse
    [
      "--full-tex", Arg.Set full_tex, "Full LaTeX file";
      "-o", Arg.Set_string file_out, "Output file";
      "-t", Arg.Set_string out_kind, "Output type"
    ]
    (fun s ->
       file_in := s;
       if !file_out = "" && Str.string_match re_file_in !file_in 0 then
         file_out := (Str.matched_group 1 !file_in) ^ ".tex"
    )
    usage;
  if !file_in = "" then
    (
      Printf.eprintf "%s\n%!" usage;
      exit 1
    );
  let out_kind =
    match !out_kind with
      | "pstricks" -> Wire.Pstricks
      | "pstricks_splines" -> Wire.Pstricks_spline
      | "tikz" -> Wire.Tikz
      | _ ->
          Printf.eprintf "Unknown output type: %s\n%!" !out_kind;
          exit 2
  in
  let sin =
    let fi = open_in !file_in in
    let flen = in_channel_length fi in
    let buf = String.create flen in
      Lang.debug (Printf.sprintf "Read %d bytes." (input fi buf 0 flen));
      close_in fi;
      buf
  in
  let env, ir = Parser.defs Lexer.token (Lexing.from_string sin) in
  let m = matrix_of_ir env ir in
  let pst = Lang.process_matrix out_kind env m in
  let fo = open_out !file_out in
    if !full_tex then
      (
        output_string fo "\\documentclass{article}\n";
        output_string fo
          (match out_kind with
             | Wire.Tikz ->
                 "\\usepackage{tikz}\n"
             | Wire.Pstricks
             | Wire.Pstricks_spline ->
                 "\\usepackage{pstricks}\n"
          );
        output_string fo "\\begin{document}\n";
        output_string fo
          (match out_kind with
             | Wire.Tikz -> "\\begin{tikzpicture}\n"
             | _ -> ""
          );
      );
    output_string fo pst;
    if !full_tex then
      (
        output_string fo
          (match out_kind with
             | Wire.Tikz -> "\\end{tikzpicture}\n"
             | _ -> ""
          );
        output_string fo "\\end{document}\n"
      );
    close_out fo
