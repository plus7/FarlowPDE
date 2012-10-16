module Program

open System

(* 練習問題3 *)
(* モンテカルロ法における試行回数 *)
let iterCnt = 10000000

(* 乱数発生器 *)
let rnd = new System.Random()

(* 0<=x<=1, 0<=y<=1, 0<=z<=1 0<=f<=1 の範囲を返す *)
let tupleSeq = Seq.initInfinite (fun index -> (rnd.NextDouble(), rnd.NextDouble(), rnd.NextDouble(), rnd.NextDouble()))

(* 「矢」をiterCnt個取ってくる |> 条件に合うものだけフィルタする |> 個数を数える *)
let hit = Seq.take iterCnt tupleSeq |>
          Seq.filter (fun (x,y,z,f) -> if f <= Math.Exp( -(x*x + y*y + z*z) ) then true else false) |>
          Seq.length

printfn "result: %f\n" ((float hit) / (float iterCnt))
