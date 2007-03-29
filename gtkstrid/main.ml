open Strid_glade

let () =
  let w = new window () in
    ignore (w#window#connect#destroy ~callback:(fun () -> GMain.Main.quit ()));
    w#window#show ();
    GMain.Main.main ()
