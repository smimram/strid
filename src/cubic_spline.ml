let loop_step = 2

let spline_periodic x y y2 u =
  (*
   Given arrays x[1..n) and y[1..n) containing a tabulated function, i.e., yi = f(xi), with
   x1 <x2 < :: : < xN, this routine returns an array y2[1..n) that contains
   the second derivatives of the interpolating function at the tabulated points xi.
   *)

  let n = Array.length x in

  (* Initilalization for n=1. *)
  let sigma = (x.(n-1)-.x.(n-2))/.(x.(1)-.x.(0) +. x.(n-1) -. x.(n-2)) in
  let p = sigma*.y2.(n-2)+.2.0 in
    y2.(0) <- (sigma-.1.0)/.p;
    u.(0) <- (y.(1)-.y.(0))/.(x.(1)-.x.(0)) -. (y.(n-1)-.y.(n-2))/.(x.(n-1)-.x.(n-2));
    u.(0) <- (6.0*.u.(0)/.(x.(1)-.x.(0) +. x.(n-1) -. x.(n-2))-.sigma*.u.(n-2))/.p;

    for i=1 to (n-2) do 
      (* This is the decomposition loop of the tridiagonal algorithm. *)
      let sigma = (x.(i)-.x.(i-1))/.(x.(i+1)-.x.(i-1)) in
      let p = sigma*.y2.(i-1)+.2.0 in
        y2.(i) <- (sigma-.1.0)/.p;
        u.(i) <- (y.(i+1)-.y.(i))/.(x.(i+1)-.x.(i)) -. (y.(i)-.y.(i-1))/.(x.(i)-.x.(i-1));
        u.(i) <- (6.0*.u.(i)/.(x.(i+1)-.x.(i-1))-.sigma*.u.(i-1))/.p;
    done;

    y2.(n-1) <- y2.(0);
    u.(n-1) <- u.(0) ;

    for k=2 to n do
      (* This is the backsubstitution loop of the tridiagonal algorithm. *)
      y2.(n-k) <- y2.(n-k)*.y2.(n-k+1)+.u.(n-k);
    done

let spline x y =
  (*
   Given arrays x[1..n) and y[1..n) containing a tabulated function, i.e., yi = f(xi), with
   x1 <x2 < :: : < xN, this routine returns an array y2[1..n) that contains
   the second derivatives of the interpolating function at the tabulated points xi.
   *)

  let n = Array.length x in
  let u = Array.make (n-1) 0. in
  let y2 = Array.make n 0. in

    for i=1 to (n-2) do 
      (*This is the decomposition loop of the tridiagonal algorithm*)
      let sigma = (x.(i)-.x.(i-1))/.(x.(i+1)-.x.(i-1)) in
      let p = sigma*.y2.(i-1)+.2.0 in
        y2.(i) <- (sigma-.1.0)/.p;
        u.(i) <- (y.(i+1)-.y.(i))/.(x.(i+1)-.x.(i)) -. (y.(i)-.y.(i-1))/.(x.(i)-.x.(i-1));
        u.(i) <- (6.0*.u.(i)/.(x.(i+1)-.x.(i-1))-.sigma*.u.(i-1))/.p;
    done;

    for k=2 to n do
      (*This is the backsubstitution loop of the tridiagonal algorithm. *)
      y2.(n-k) <- y2.(n-k)*.y2.(n-k+1)+.u.(n-k);
    done;

    y2

let make_interp x y periodic =
  if periodic then 
    (
      let y2 = Array.make (Array.length x) 0. in
      let aux = Array.make (Array.length x) 0. in
        for i = 1 to loop_step do
          spline_periodic x y y2 aux
        done;
        (x,y,y2)
    )
  else
    (x,y,spline x y)



let eval (xa,ya,y2a) x =
  (*
   Given the arrays xa[1..n] and ya[1..n], which tabulate a function (with the xai's in order),
   and given the array y2a[1..n], which is the output from spline above, and given a value of
   x, this routine returns a cubic-spline interpolated value y.
   *)
  let klo = ref 0 in 
  let khi = ref ((Array.length xa) - 1) in 
  let k = ref 0 in
    while ((!khi - !klo) > 1)  do
      k := (!khi + !klo) / 2 ;
      if xa.(!k) > x then
        khi := !k
      else
        klo := !k
    done;
    (*klo and khi now bracket the input value of x.*)
    let h = xa.(!khi)-.xa.(!klo) in
    let a = (xa.(!khi)-.x)/.h in
    let b = (x-.xa.(!klo))/.h in
      (*Cubic spline polynomial is now evaluated.*)
      a*.ya.(!klo)+.b*.ya.(!khi)+.((a*.a*.a-.a)*.y2a.(!klo)+.(b*.b*.b-.b)*.y2a.(!khi))*.(h*.h)/.6.0
