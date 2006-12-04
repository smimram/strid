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

(**
  * Cubic splines interpolation.
  *
  * @authors Samuel Mimram and Nicolas Tabareau
  *)

let iterations_nb = 11

(**
  * Given arrays x and y containing a tabulated function, i.e. y_i = f(x_i), with
  * x_1 < x_2 < ... < x_n, this routine returns an array that contains
  * the second derivatives of the interpolating function at the tabulated points
  * x_i.
  *)
let spline x y =
  let n = Array.length x in
  let u = Array.make (n-1) 0. in
  let y2 = Array.make n 0. in
    for i=1 to (n-2) do
      (* This is the decomposition loop of the tridiagonal algorithm. *)
      let sigma = (x.(i)-.x.(i-1))/.(x.(i+1)-.x.(i-1)) in
      let p = sigma*.y2.(i-1)+.2.0 in
        y2.(i) <- (sigma-.1.0)/.p;
        u.(i) <- (y.(i+1)-.y.(i))/.(x.(i+1)-.x.(i)) -. (y.(i)-.y.(i-1))/.(x.(i)-.x.(i-1));
        u.(i) <- (6.0*.u.(i)/.(x.(i+1)-.x.(i-1))-.sigma*.u.(i-1))/.p;
    done;
    for k = 2 to n do
      (* This is the backsubstitution loop of the tridiagonal algorithm. *)
      y2.(n-k) <- y2.(n-k)*.y2.(n-k+1)+.u.(n-k);
    done;
    y2

let spline_periodic x y =
  let n = (Array.length x) - 1 in
  let big_x = Array.init (iterations_nb * n) (fun i -> x.(i mod n) +. x.(n) *. (float_of_int (i / n))) in
  let big_y = Array.init (iterations_nb * n) (fun i -> y.(i mod n)) in
  let y2 = spline big_x big_y in
  let y2 = Array.sub y2 ((iterations_nb/2)*n) (n + 1) in
    y2

let make_interp x y periodic =
  if periodic then
    (x,y,spline_periodic x y)
  else
    (x,y,spline x y)

(*
 * Given the arrays xa and ya, which tabulate a function (with the xa_i's in order),
 * and given the array y2a, which is the output from spline above, and given a value of
 * x, this routine returns a cubic-spline interpolated value y.
 *)
let eval (xa, ya, y2a) x =
  let klo = ref 0 in
  let khi = ref ((Array.length xa) - 1) in
  let k = ref 0 in
    while ((!khi - !klo) > 1)  do
      k := (!khi + !klo) / 2;
      if xa.(!k) > x then
        khi := !k
      else
        klo := !k
    done;
    (* klo and khi now bracket the input value of x. *)
    let h = xa.(!khi)-.xa.(!klo) in
    let a = (xa.(!khi)-.x)/.h in
    let b = (x-.xa.(!klo))/.h in
      (* Cubic spline polynomial is now evaluated. *)
      a*.ya.(!klo)+.b*.ya.(!khi)+.((a*.a*.a-.a)*.y2a.(!klo)+.(b*.b*.b-.b)*.y2a.(!khi))*.(h*.h)/.6.0
