// Learn more about F# at http://fsharp.org

open System
open FSRP.Core
open FSRP.Signal
open ShooterGame

[<EntryPoint>]
let main argv =
    
    let rec unfold (Unfold(eval)) =
        let (out, eval') = eval ()
        printfn "%A" out
        unfold eval'

    let evaluator = buildUnfolder (zeros ())
    //unfold evaluator

    start ()
   
    //(unfold_signal (leakyNats ()) output 0)
    0
