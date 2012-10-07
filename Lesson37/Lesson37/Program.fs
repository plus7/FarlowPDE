module Program
(* ファーロウ本 37課 練習問題6 *)

(*
正方形内部のDirichlet問題
  u_xx + u_yy = f(x,y) //0<x<1, 0<y<1
  u(x,y) = g(x,y)      //境界上で
を解くブログラム.
*)

open System
open System.Drawing
open Microsoft.FSharp.Math.SI
open Microsoft.FSharp.Math.PhysicalConstants

(* 単位の定義 *)
[<Measure>] type degC //温度

let dirichletProblemSolver m n (h:float) (f:int->int->int->int->float<degC>) (g:int->int->int->int->float<degC>) initial_u iter =
  let iteration (u:float<degC> [,]) =
    Array2D.mapi (fun i j v ->
                    if i = 0 || j = 0 || i = m-1 || j = n-1 then
                      g i j m n
                    else
                      //近傍の4点
                      let neighbor = u.[i,j+1] + u.[i,j-1] + u.[i+1,j] + u.[i-1,j]
                      //熱源
                      let force = h*h*(f i j m n)
                      (neighbor - force) / 4.0
                 ) u
  let rec solver u iter =
    if iter = 0 then u
    else solver (iteration u) (iter-1)
  solver initial_u iter
  
(* x方向分割数 *)
let n = 100
(* y方向分割数 *)
let m = 100
(* 時間の刻み幅 *)
let h = 0.1
(* 初期値 *)
let ini_u = Array2D.create m n 0.0<degC>
(* 非同次項。今回は0にしたので結局Laplace方程式である。 *)
let f i j m n = 0.0<degC>
(* 境界条件 *)
let g i j m n = if i = 0 then Math.Sin(Math.PI * (float j) / (float n)) * 1.0<degC> else 0.0<degC>
(* 結果 *)
let result = dirichletProblemSolver m n h f g ini_u 10000

(* 結果の出力 *)
let bmp = new Bitmap (n, m)

(* 温度を色に変換 *)
let degreeToGray deg =
  let nodim = deg/1.0<degC>
  let gray = int (nodim * 255.0)
  Color.FromArgb (gray, gray, gray)

(* 結局for文で書いた *)
for i = 0 to m - 1 do
  for j = 0 to n - 1 do
    bmp.SetPixel (j, i, (degreeToGray result.[i,j]))
    
bmp.Save "result.bmp"
