module FRPLibraryTest

open FSRP.Core
open FSRP.Library
open FSharp.Reflection
open System

let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next (10000))

let test_signal = ruin_gc 1337

//[<EntryPoint>]
let main argv =
    let mutable i = 0
    let input () = 
         (genRandomNumbers 10)


    let output (current: int) = printfn "%A" current
        //if current % 10 = 0 then printfn "%A" current
    

    (unfold_signal test_signal output)
    0