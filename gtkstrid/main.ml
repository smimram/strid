open Strid_glade

let about () =
  let md =
      GWindow.about_dialog
      ~authors:["Samuel Mimram"]
      ~comments:"A GTK interface for strid."
      ~copyright:"Copyright (C) 2006-2007 Samuel Mimram"
      ~license:"This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA."
      ~name:"gtkstrid"
      ~version:"0.1.0β"
      ~website:"http://www.pps.jussieu.fr/~smimram/strid/"
      ()
  in
    try
      ignore (md#run ());
      md#destroy ()
    with
      | Not_found -> md#destroy ()

let () =
  let w = new window () in
    ignore (w#window#connect#destroy ~callback:GMain.Main.quit);
    ignore (w#menu_quit#connect#activate ~callback:GMain.Main.quit);
    ignore (w#menu_about#connect#activate ~callback:about);
    w#window#show ();
    GMain.Main.main ()
