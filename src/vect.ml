let norm (x,y) =
  sqrt (x *. x +. y *. y)

let normalize (x,y) =
  let n = norm (x,y) in
    (x /. n, y /. n)

let orthogonal (x,y) =
  if y = 0. then
    (0., 1.)
  else
    normalize (1., -.x/.y)

let scale l (x, y) =
  l *. x, l *. y

let add (x,y) (x',y') =
  x +. x', y +. y'

let sub (x,y) (x',y') =
  x -. x', y -. y'

let distance p q =
  norm (sub q p)

let ( +@ ) = add

let ( -@ ) = sub

let ( *@ ) = scale
