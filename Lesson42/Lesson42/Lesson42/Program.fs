module Program

open System

(* 練習問題1 *)
(* モンテカルロ法における試行回数 *)
let iterCnt = 10000000

(* 自然対数の底 *)
let e = System.Math.E

(* 乱数発生器 *)
let rnd = new System.Random()

(* 0<=x<=1, 0=<y<=e の範囲を返す *)
let tupleSeq = Seq.initInfinite (fun index -> (rnd.NextDouble(), rnd.NextDouble()*e))

(* 「矢」をiterCnt個取ってくる |> 条件に合うものだけフィルタする |> 個数を数える *)
let hit = Seq.take iterCnt tupleSeq |>
          Seq.filter (fun (x,y) -> if y <= Math.Exp(Math.Sin(x)) then true else false) |>
          Seq.length

printfn "result: %f\n" ((float hit) / (float iterCnt))
