module FRPLibraryTest

open FSRP.Core
open FSRP.Library


let genRandomNumbers count =
    let rnd = System.Random()
    List.init count (fun _ -> rnd.Next (10000))

//[<EntryPoint>]
let main argv =
    let mutable i = 0
    let input () = 
         genRandomNumbers 10

    
    let output (current: int) = ()
        //if current % 10 = 0 then printfn "%A" current
    

    //(executor (unbox (test2())) input output)
    0