let deffound v f =
  try f () with Not_found -> v
