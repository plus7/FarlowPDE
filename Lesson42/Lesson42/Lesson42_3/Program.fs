module Program

open System

(* 練習問題6 *)
(* モンテカルロ法における試行回数 *)
let iterCnt = 10000000

(* Y座標を考えなくても一般性を失わない(はず)*)

(* 乱数発生器 *)
let rnd = new System.Random()

(* 縞の数 *)
let stripes = 50

(* 0<=x<=縞の数, 0<=theta<=π の範囲を返す *)
(* xは針の中心の座標、thetaは針の回転角度 *)
let tupleSeq = Seq.initInfinite (fun index -> (rnd.NextDouble()*(float stripes), rnd.NextDouble()*Math.PI))

(* 「矢」をiterCnt個取ってくる |> 条件に合うものだけフィルタする |> 個数を数える *)
let hit = Seq.take iterCnt tupleSeq |>
          Seq.filter (fun (x, theta) ->
                        if Math.Floor(x + 0.5*Math.Cos(theta)) <> Math.Floor(x - 0.5*Math.Cos(theta)) then
                          true
                        else
                          false) |>
          Seq.length

printfn "result: %f\nreference: 2/π=%f\n" ((float hit) / (float iterCnt)) (2.0/Math.PI)
