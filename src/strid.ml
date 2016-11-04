(*
 * Copyright (C) 2006 Samuel Mimram
 *
 * This file is part of strid.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Lang

let re_file_in = Str.regexp "\\(.*\\)\\.strid"
let file_in = ref []
let file_out = ref ""
let xelatex = ref false
let pdf_output = ref false
let ps_output = ref false
let dump_conf = ref false
let out_kind = ref Wire.Tikz
let graphics_refresh = ref false
let full_tex = ref false
let standalone_tex = ref false
let latex_preamble = ref ""
let latex_postamble = ref ""
let conf = ref Conf.fname

let get_pos d i j =
  (i*10, j*10)

let kind_of_string = function
  | "pstricks" -> Wire.Pstricks
  | "tikz" -> Wire.Tikz
  | "graphics" -> Wire.Graphics
  | s ->
     Printf.eprintf "Unknown output type: %s\n%!" s;
     exit 2

let parse_file f =
  let sin =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = String.create flen in
    really_input fi buf 0 flen;
    Common.debug (Printf.sprintf "Read %d bytes." flen);
    close_in fi;
    buf
  in
  let ir =
    let lexbuf = Lexing.from_string sin in
    try
      Parser.defs Lexer.token lexbuf
    with
    | Failure "lexing: empty token" ->
       let pos = (Lexing.lexeme_end_p lexbuf) in
       let err =
         Printf.sprintf
           "Lexing error at line %d, character %d."
           pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
       in
       if !out_kind = Wire.Graphics then
         failwith err
       else
         Common.error err
    | Parsing.Parse_error ->
       let pos = (Lexing.lexeme_end_p lexbuf) in
       let err =
         Printf.sprintf
           "Parse error at word \"%s\", line %d, character %d."
           (Lexing.lexeme lexbuf)
           pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
       in
       if !out_kind = Wire.Graphics then
         failwith err
       else
         Common.error err
    | Invalid_box e ->
       let pos = (Lexing.lexeme_end_p lexbuf) in
       let err =
         Printf.sprintf
           "Error at word \"%s\", line %d, character %d: %s"
           (Lexing.lexeme lexbuf)
           pos.Lexing.pos_lnum
           (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
           e
       in
       if !out_kind = Wire.Graphics then
         failwith err
       else
         Common.error err
  in
  matrix_of_ir ir

let usage = "strid -- A string diagrams generator\nusage: strid [options] file"

let () =
  if Conf.exists !conf then
    (
      Conf.read! conf;
      Common.info (Printf.sprintf "Read configuration file %s." !conf)
    );
  Arg.parse
    (Arg.align
       [
         "--config", Arg.Set_string conf, " Use a specific configuration file";
         "--dump-conf", Arg.Set dump_conf, (" Dump configuration file in " ^ Conf.fname);
         "--pdf", Arg.Set pdf_output, " Generate a pdf file";
         "--ps", Arg.Set ps_output, " Generate a postscript file";
         "-g", Arg.Unit (fun () -> out_kind := Wire.Graphics; graphics_refresh := true), " Use Graphics output";
         "--latex-full", Arg.Set full_tex, " Full LaTeX file";
         "--latex-standalone", Arg.Set standalone_tex, " LaTeX file with standalone class.";
         "--latex-no-environment", Arg.Unit (fun () -> Conf.set_bool "no_tex_environment" true), " Don't output LaTeX environment";
         "--latex-preamble", Arg.Set_string latex_preamble, " LaTeX preamble";
         "--latex-postamble", Arg.Set_string latex_postamble, " LaTeX postamble";
         "-o", Arg.Set_string file_out, " Output file";
         "--scale", Arg.Float (fun f -> Conf.set_float "scaling_factor" f), " Scale the output";
         "--show-grid", Arg.Unit (fun () -> Conf.set_bool "show_grid" true), " Show grid points";
         "-t", Arg.String (fun s -> out_kind := kind_of_string s), " Output type";
         "--xelatex", Arg.Set xelatex, " Use XeLaTeX";
       ]
    )
    (fun s -> file_in := s::!file_in)
    usage;
  if !full_tex then Conf.set_bool "no_tex_environment" false;
  if !dump_conf then
    (
      if Conf.exists !conf then
        (
          Conf.read !conf;
          Conf.save !conf;
          Common.info (Printf.sprintf "Configuration file %s updated." !conf)
        )
      else
        (
          Conf.save !conf;
          Common.info (Printf.sprintf "Configuration file saved in %s." !conf)
        );
      exit 0
    );
  if !file_in = [] then
    (
      Printf.eprintf "%s\n%!" usage;
      exit 1
    );
  List.iter
    (fun fname_in ->
     match !out_kind with
     | Wire.Graphics ->
        let loop = ref true in
        let reload = ref true in
        let last_mtime = ref 0. in
        Graphics.open_graph "";
        Graphics.resize_window 100 100;
        while !loop do
          if !reload then
            (
              try
                Conf.reset ();
                if Conf.exists !conf then Conf.read !conf;
                let m = parse_file fname_in in
                Graphics.auto_synchronize false;
                Graphics.clear_graph ();
                ignore (Lang.process_matrix !out_kind m);
                Graphics.set_window_title ("Strid - " ^ fname_in);
                Graphics.auto_synchronize true;
                reload := false
              with
              | e ->
                 Common.warning (Printf.sprintf "Ignoring error: %s." (Printexc.to_string e));
                 reload := false
            );
          if !graphics_refresh then
            try
              Unix.sleep 1;
              let mtime = (Unix.stat fname_in).Unix.st_mtime in
              if mtime <> !last_mtime then
                (
                  last_mtime := mtime;
                  reload := true
                )
            with
            | Unix.Unix_error(Unix.ENOENT, _, _) -> () (* file not found *)
            | e ->
               Common.warning (Printexc.to_string e)
          else
            ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
          if Graphics.key_pressed () then
            let k = Graphics.read_key () in
            if k = 'q' then
              loop := false
            else if k = 'r' then
              reload := true
        done;
        Graphics.close_graph ()
     | _ ->
        let m = parse_file fname_in in
        let pst = Lang.process_matrix !out_kind m in
        let fname_out =
          if List.length !file_in = 1 && !file_out <> "" then
            (* TODO: handle .pdf when in pdf_output mode *)
            !file_out
          else
            if !file_out = "" && Str.string_match re_file_in fname_in 0 then
              Str.matched_group 1 fname_in ^ ".tex"
            else
              Common.error (Printf.sprintf "Invalid input file name: %s.\n" fname_in)
        in
        let fo = open_out fname_out in
        if !ps_output then
          pdf_output := true;
        if !full_tex || !standalone_tex || !pdf_output then
          (
            if !standalone_tex then
              output_string fo "\\documentclass[tikz]{standalone}\n"
            else
              output_string fo "\\documentclass{article}\n";
            output_string fo
                          (match !out_kind with
                           | Wire.Tikz ->
                              "\\usepackage{tikz}\n" ^
                                (
                                  if !pdf_output then
                                    "\\usepackage[active,tightpage]{preview}\n\\PreviewEnvironment{tikzpicture}\n"
                                  else ""
                                )
                           | Wire.Pstricks ->
                              "\\usepackage{pstricks}\n"
                           | Wire.Graphics -> ""
                          );
            output_string fo (!latex_preamble ^ "\n\\begin{document}\n\\pagestyle{empty}\n" ^ !latex_postamble ^ "\n");
          );
        output_string fo pst;
        if !full_tex || !standalone_tex || !pdf_output then
          output_string fo "\\end{document}\n";
        close_out fo;
        Common.info (Printf.sprintf "Successfully generated %s." fname_out);
        let fname_out_dir = Filename.dirname fname_out in
        let fname_out_chopped = Filename.chop_extension fname_out in
        let fname_out_pdf = fname_out_chopped ^ ".pdf" in
        if !pdf_output then
          (
            let pdflatex = if !xelatex then "xelatex" else "pdflatex" in
            if
              (Sys.command (Printf.sprintf "%s -halt-on-error -output-directory '%s' '%s' > /dev/null" pdflatex fname_out_dir fname_out)) <> 0
            then
              Common.error (Printf.sprintf "Error while compiling %s.\nThe error is likely to be indicated at the end of %s.log.\n(forgotten macro definition?)" fname_out fname_out_chopped);
            assert ((Sys.command (Printf.sprintf "rm -f '%s' '%s.log' '%s.aux' strid.latex.log" fname_out fname_out_chopped fname_out_chopped)) = 0);
            Common.info (Printf.sprintf "Successfully generated %s." fname_out_pdf)
          );
        (* TODO: improve this... *)
        if !ps_output then
          let fname_out_ps = fname_out_chopped ^ ".ps" in
          assert (Sys.command (Printf.sprintf "echo \"%%!PS-Adobe-3.0\" > '%s'" fname_out_ps) = 0);
          assert (Sys.command (Printf.sprintf "echo -n \"%%%%BoundingBox: \" >> '%s'" fname_out_ps) = 0);
          assert (Sys.command (Printf.sprintf "echo \"`cat '%s' | grep -a MediaBox | sed 's/\\/MediaBox \\[\\(.*\\)\\]/\\1/'`\" >> '%s'" fname_out_pdf fname_out_ps) = 0);
          assert (Sys.command (Printf.sprintf "pdftops '%s' - | grep -v \"PS-Adobe-3.0\" | grep -v \"Produced by xpdf\" | grep -v \"DocumentMedia\" | grep -v \"translate\" >> '%s'" fname_out_pdf fname_out_ps) = 0);
          Sys.remove fname_out_pdf;
          Common.info (Printf.sprintf "Successfully generated %s." fname_out_ps)
    ) !file_in
