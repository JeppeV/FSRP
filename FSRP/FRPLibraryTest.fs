module FRPLibraryTest

open FSRP.Core
open FSRP.Signal
open FSharp.Reflection
open System
open FSRPTests

open ShooterGame

let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next (10000))

//let test_signal = test (box (lazy(delay(lazy(1)))))

[<EntryPoint>]
let main argv =
    let mutable i = 0
    let input () = 
         genRandomNumbers 2


    let output (current: int) = printfn "%A" current; true
        //if current % 10 = 0 then printfn "%A" current

    
    let rec unfold (Unfold(eval)) =
        let (out, eval') = eval ()
        printfn "%A" out
        unfold eval'

    let evaluator = buildUnfolder (zeros ())
    //unfold evaluator

    run()
    //(unfold_signal (leakyNats ()) output 0)
    0