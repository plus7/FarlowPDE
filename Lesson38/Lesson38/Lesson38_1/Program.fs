module Program

(* ファーロウ本 38課 練習問題1, 2 *)

(*
熱伝導問題
  u_t = u_xx            // 0<x<1, 0<t<INF
  u(0,t) = 0            // 0<t<INF
  u(1,t) = 0            // 0<t<INF
  u(x,0) = sin(π * x)   // 0<=x<=1
を解くブログラム.
*)

open System
open System.Drawing
open Microsoft.FSharp.Math.SI
open Microsoft.FSharp.Math.PhysicalConstants

(* 単位の定義 *)
[<Measure>] type degC //温度

let heatXferSolver (m:int) (n:int) (g: float<s> -> float<degC>) =
  let h = 1.0 / (float n)
  let k = 1.0 / (float m)
  let r = k / (h * h)
  let u = Array2D.create (m+1) (n+1) 0.0<degC>
  for j = 0 to n do
      u.[0, j] <- Math.Sin( Math.PI * (float j) / (float n) ) * 1.0<degC>
  let rec calculate (xdiv:int) (tcurr:int) (tmax:int) (u: float<degC> [,]) =
    if tcurr = tmax then
      u
    else
      for j = 1 to xdiv - 1 do
        u.[tcurr+1, j] <- u.[tcurr, j] + r * (u.[tcurr, j+1] - 2.0 * u.[tcurr,j] + u.[tcurr, j-1])
      calculate xdiv (tcurr+1) tmax u 
  calculate n 0 (m-1) u

(* 解析解は u(x, t) = exp(-PI^2*t) sin(PI*x) *)
let heatXferAnalyticalSolver (m:int) (n:int) =
  let u = Array2D.create (m+1) (n+1) 0.0<degC>
  for i = 0 to m do
    let t = (float i) / (float m)
    for j = 0 to n do
      let x = (float j) / (float n)
      u.[i,j] <- Math.Exp( - Math.PI * Math.PI * t ) * Math.Sin( Math.PI * x) * 1.0<degC>
  u

(* x方向分割数 *)
let n = 10
(* t方向分割数 *)
let m = 200

(* 結果 *)
let result = heatXferSolver m n (fun x -> 0.0<degC>)
let result2 = heatXferAnalyticalSolver m n

(* 温度を色に変換 *)
let degreeToGray deg =
  let nodim = deg/1.0<degC>
  let gray = Math.Max(0, Math.Min(255, int (nodim * 255.0)))
  Color.FromArgb (gray, gray, gray)

(* 結果の出力 *)
let saveToBmp n m (u: float<degC> [,]) filename = 
  let bmp = new Bitmap (n+1, m+1)
  (* 結局for文で書いた *)
  for i = 0 to m do
    for j = 0 to n do
      bmp.SetPixel (j, i, (degreeToGray u.[i,j]))  
  bmp.Save filename
  
saveToBmp n m result "result.bmp"
saveToBmp n m result2 "result2.bmp"
