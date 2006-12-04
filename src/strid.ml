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
      "--dump-conf", Arg.Set dump_conf, ("Dump configuration file in " ^ Conf.fname);
      "--full-tex", Arg.Set full_tex, "Full LaTeX file";
      "--no-tex-environment", Arg.Set Env.no_tex_environment, "Don't output LaTeX environment";
      "-o", Arg.Set_string file_out, "Output file";
      "--scale", Arg.Set_float Env.scaling_factor, "Scale the output";
      "-t", Arg.Set_string out_kind, "Output type"
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
