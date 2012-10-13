module Program

open System
open System.Drawing
open Microsoft.FSharp.Math.SI
open Microsoft.FSharp.Math.PhysicalConstants

(*
ファーロウ本 39課 練習問題5
  偏微分方程式:
    u_t = u_xx (0<x<1, 0<t<∞)
  境界条件:
    u(0,t) = 0 (0<=t<∞)
    u(1,t) = 0
  初期条件:
    u(x,0) = sin(πx) (0<x<1)
を陰的差分法で解く。
また、解析解である
u(x,t) = exp(-π^2*t)*sin(πx)
と比較する。
*)

(* 今回は温度にKを使ってみた *)

(* 時間方向の格子点の数 *)
let m = 100
(* i=m-1 のときの時間 *)
let tmax = 1.0<s>
(* X方向の格子点の数 *)
let n = 100
(* j=n-1のときの時間 *)
let xmax = 1.0<m>
(* (x,t)での温度u *)
let u = Array2D.create m n 0.0<K>
(* 三重対角系の係数行列 *)
let a = Array.create (n-3) 0.0<s/m^2>
let b = Array.create (n-2) 0.0<s/m^2>
let c = Array.create (n-3) 0.0<s/m^2>
(* 三重対角系の定数。t=iのときのuの値で決められる。 *)
let d = Array.create (n-2) 0.0<K*s/m^2>
(*
三重対角系
tridiag * x = d
のxを求める。
なお、x.[j] は u.[i+1,j+1]に等しい
*)

(* Crank-Nicolson法のパラメータ *)
let lambda = 0.5

(* 渡された関数fに基づいてuに初期値を代入 *)
let set_initial_val (u: float<K> [,]) (n:int) (xmax:float<m>) (f:float<m>->float<K>) =
  for j = 0 to n-1 do
    u.[0, j] <- f (xmax * (float j) / (float (n-1)))

(* 対角成分を1+2rλで、その一つ脇を-λrで初期化 *)
let set_tridiag_mat (a: float<'u>[]) (b: float<'u>[]) (c: float<'u>[]) (r:float<'u>) (lambda:float) =
  for i = 0 to a.Length - 1 do
    a.[i] <- -r*lambda
    c.[i] <- -r*lambda
  for i = 0 to b.Length - 1 do
    b.[i] <- 1.0<_> + 2.0*r*lambda

(* d_1 ～ d_(n-2) を埋める。配列がゼロオリジンなのでちょっとずれることに注意 *)
let set_d (u:float<K> [,]) (d:float<K*s/m^2> []) (i:int) (r:float<s/m^2>) (lambda:float) =
  for j = 0 to d.Length-1 do
    if j = 0 then
      d.[0] <-                           (1.0<_> - 2.0*r*(1.0-lambda))*u.[i, 1]   + r*(1.0-lambda)*u.[i, 2]
    else if j = d.Length-1 then
      d.[j] <- r*(1.0-lambda)*u.[i, j] + (1.0<_> - 2.0*r*(1.0-lambda))*u.[i, j+1]
    else
      d.[j] <- r*(1.0-lambda)*u.[i, j] + (1.0<_> - 2.0*r*(1.0-lambda))*u.[i, j+1] + r*(1.0-lambda)*u.[i, j+2]

(* 係数行列を使いやすいように変換する *)
let transform (a: float<'u>[]) (b: float<'u>[]) (c: float<'u>[]) (d: float<'t>[]) =
  let n = b.Length
  let newc = Array.create (n-1) 0.0
  let newd = Array.create n 0.0<_>
  
  newc.[0] <- c.[0] / b.[0]
  for i = 1 to c.Length - 1 do
    newc.[i] <- c.[i] / (b.[i] - a.[i-1]*newc.[i-1])

  newd.[0] <- d.[0] / b.[0]
  for i = 1 to d.Length - 1 do
    newd.[i] <- (d.[i] - a.[i-1]*newd.[i-1]) / (b.[i] - a.[i-1]*newc.[i-1])

  (newc, newd)

(* 時間が⊿xだけ経過した時の温度を計算する *)
let solve (u:float<K> [,]) (c: float<'u>[]) (d: float<K>[]) (i:int) =
  let n = d.Length - 1
  u.[i+1, n+1] <- d.[n]
  for j = n-1 downto 1 do
    u.[i+1, j+1] <- d.[j] - c.[j]*u.[i+1, j+2]
    
(* 解析解 *)
let reference (m:int) (n:int) (tmax:float<s>) (xmax:float<m>) =
  let u = Array2D.create m n 0.0<K>
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      let t = tmax*(float i)/(float (m-1))
      let x = xmax*(float j)/(float (n-1))
      //u(x,t) = exp(-π^2*t)*sin(πx)
      u.[i,j] <- 1.0<K> * Math.Exp(-Math.PI*Math.PI*(float t)) * Math.Sin(Math.PI * (float x))
  u

(* 温度を色に変換 *)
let temperatureToColor t =
  let nodim = (float t)
  if nodim > 1.0 then
    Color.Red
  else if nodim < 0.0 then
    Color.Blue
  else
    let gray = int (nodim * 255.0)
    Color.FromArgb (gray, gray, gray)

(* 結果の出力 *)
let saveToBmp (m:int) (n:int) (u: float<K> [,]) filename = 
  let bmp = new Bitmap (m, n)
  (* 結局for文で書いた *)
  for i = 0 to m-1 do
    for j = 0 to n-1 do
      bmp.SetPixel (i, j, (temperatureToColor u.[i,j]))  
  bmp.Save filename

do
  set_initial_val u n xmax (fun x -> 1.0<K> * Math.Sin( Math.PI * (float x) ))
  let k = tmax/(float (m-1))
  let h = xmax/(float (n-1))
  let r = k / (h*h)
  for i = 0 to m-2 do
    set_tridiag_mat a b c r lambda
    set_d u d i r lambda
    let (newc, newd) = transform a b c d
    solve u newc newd i
  saveToBmp m n u "answer.bmp"
  let ref_u = reference m n tmax xmax
  saveToBmp m n ref_u "reference.bmp"