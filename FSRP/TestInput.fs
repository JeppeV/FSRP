module TestInput

open FSRP.Core
open FSRP.Library



let test = 
    let HEREHEHRHEHRE = fun (s: Signal<int>) -> s
    HEREHEHRHEHRE

let rec test2 = 
   1
and test3 = 
    ""


let rec test4 (s: Signal<int>) x = match s with | hd :: tl -> hd :: delay(lazy (test4 (adv tl) x))


