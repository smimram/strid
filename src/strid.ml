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
let file_in = ref ""
let file_out = ref ""
let full_tex = ref false
let dump_conf = ref false
let out_kind = ref "tikz"

let get_pos d i j =
  (i*10, j*10)

let usage = "strid -- A string diagrams generator\nusage: strid [options] file"

let _ =
  Arg.parse
    [
      "--dump-conf", Arg.Set dump_conf, ("\t\tDump configuration file in " ^ Conf.fname);
      "--full-tex", Arg.Set full_tex, "\t\tFull LaTeX file";
      "--no-tex-environment", Arg.Unit (fun () -> Conf.set_bool "no_tex_environment" true), "\tDon't output LaTeX environment";
      "-o", Arg.Set_string file_out, "\t\t\tOutput file";
      "--scale", Arg.Float (fun f -> Conf.set_float "scaling_factor" f), "\t\tScale the output";
      "-t", Arg.Set_string out_kind, "\t\t\tOutput type"
    ]
    (fun s ->
       file_in := s;
       if !file_out = "" && Str.string_match re_file_in !file_in 0 then
         file_out := (Str.matched_group 1 !file_in) ^ ".tex"
    )
    usage;
  if !dump_conf then
    (
      Conf.save Conf.fname;
      Common.info (Printf.sprintf "Configuration file saved in %s." Conf.fname);
      exit 0
    );
  if Conf.exists Conf.fname then
    (
      Conf.read Conf.fname;
      Common.info (Printf.sprintf "Read configuration file %s." Conf.fname)
    );
  if !file_in = "" then
    (
      Printf.eprintf "%s\n%!" usage;
      exit 1
    );
  let out_kind =
    match !out_kind with
      | "pstricks" -> Wire.Pstricks
      | "tikz" -> Wire.Tikz
      | _ ->
          Printf.eprintf "Unknown output type: %s\n%!" !out_kind;
          exit 2
  in
  let sin =
    let fi = open_in !file_in in
    let flen = in_channel_length fi in
    let buf = String.create flen in
      Common.debug (Printf.sprintf "Read %d bytes." (input fi buf 0 flen));
      close_in fi;
      buf
  in
  let env, ir =
    let lexbuf = Lexing.from_string sin in
      try
        Parser.defs Lexer.token lexbuf
      with
        | Failure "lexing: empty token" ->
            let pos = (Lexing.lexeme_end_p lexbuf) in
              Common.error
                (Printf.sprintf "Lexing error at line %d, character %d."
                   pos.Lexing.pos_lnum
                   (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
                )
        | Parsing.Parse_error ->
            let pos = (Lexing.lexeme_end_p lexbuf) in
              Common.error
                (Printf.sprintf "Parse error at word \"%s\", line %d, character %d."
                   (Lexing.lexeme lexbuf)
                   pos.Lexing.pos_lnum
                   (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
                )
  in
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
             | Wire.Pstricks ->
                 "\\usepackage{pstricks}\n"
          );
        output_string fo "\\begin{document}\n";
      );
    output_string fo pst;
    if !full_tex then
      output_string fo "\\end{document}\n";
    close_out fo;
    Common.info (Printf.sprintf "Successfully generated %s." !file_out)
