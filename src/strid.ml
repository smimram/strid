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
let pdf_output = ref false
let dump_conf = ref false
let out_kind = ref Wire.Tikz
let graphics_refresh = ref false
let full_tex = ref false
let latex_preamble = ref ""

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
      Common.debug (Printf.sprintf "Read %d bytes." (input fi buf 0 flen));
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
              Printf.sprintf "Lexing error at line %d, character %d."
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
              Printf.sprintf "Parse error at word \"%s\", line %d, character %d."
                (Lexing.lexeme lexbuf)
                pos.Lexing.pos_lnum
                (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
            in
              if !out_kind = Wire.Graphics then
                failwith err
              else
                Common.error err
  in
    matrix_of_ir ir

let usage = "strid -- A string diagrams generator\nusage: strid [options] file"

let _ =
  Arg.parse
    [
      "--dump-conf", Arg.Set dump_conf, ("\t\t\tDump configuration file in " ^ Conf.fname);
      "--pdf", Arg.Set pdf_output, "\t\t\tGenerate a pdf file";
      "-g", Arg.Unit (fun () -> out_kind := Wire.Graphics; graphics_refresh := true), "\t\t\t\tUse Graphics output";
      "--latex-full", Arg.Set full_tex, "\t\t\tFull LaTeX file";
      "--latex-no-environment", Arg.Unit (fun () -> Conf.set_bool "no_tex_environment" true), "\tDon't output LaTeX environment";
      "--latex-preamble", Arg.Set_string latex_preamble, "\t\tLaTeX preamble";
      "-o", Arg.Set_string file_out, "\t\t\t\tOutput file";
      "--scale", Arg.Float (fun f -> Conf.set_float "scaling_factor" f), "\t\t\tScale the output";
      "-t", Arg.String (fun s -> out_kind := kind_of_string s), "\t\t\t\tOutput type"
    ]
    (fun s -> file_in := s::!file_in)
    usage;
  if !dump_conf then
    (
      if Conf.exists Conf.fname then
        (
          Conf.read Conf.fname;
          Conf.save Conf.fname;
          Common.info (Printf.sprintf "Configuration file %s updated." Conf.fname)
        )
      else
        (
          Conf.save Conf.fname;
          Common.info (Printf.sprintf "Configuration file saved in %s." Conf.fname)
        );
      exit 0
    );
  if Conf.exists Conf.fname then
    (
      Conf.read Conf.fname;
      Common.info (Printf.sprintf "Read configuration file %s." Conf.fname)
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
                 !file_out
               else
                 if !file_out = "" && Str.string_match re_file_in fname_in 0 then
                   Str.matched_group 1 fname_in ^ ".tex"
                 else
                   Common.error (Printf.sprintf "Invalid input file name: %s.\n" fname_in)
             in
             let fo = open_out fname_out in
               if !full_tex || !pdf_output then
                 (
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
                   output_string fo ("\\begin{document}\n\\pagestyle{empty}\n" ^ !latex_preamble ^ "\n");
                 );
               output_string fo pst;
               if !full_tex || !pdf_output then
                 output_string fo "\\end{document}\n";
               close_out fo;
               Common.info (Printf.sprintf "Successfully generated %s." fname_out);
               if !pdf_output then
                 let fname_out_chopped = Filename.chop_extension fname_out in
                 let fname_out_pdf = fname_out_chopped ^ ".pdf" in
                   assert ((Sys.command (Printf.sprintf "pdflatex '%s' > /dev/null" fname_out)) = 0);
                   assert ((Sys.command (Printf.sprintf "rm -f '%s' '%s.log' '%s.aux'" fname_out fname_out_chopped fname_out_chopped)) = 0);
                   Common.info (Printf.sprintf "Successfully generated %s." fname_out_pdf)
    ) !file_in
