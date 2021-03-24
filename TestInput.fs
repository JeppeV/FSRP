module TestInput

open FSRP.Core
open FSRP.Library

//[<SF>]
//let rec con (a: Box<'A>) () = 
//    let l = lazy(3)
//    Signal((unbox(a), delay(lazy(con a ()))))

//[<SF>]
//let rec map (f: Box<'A -> 'B>) (s: Signal<'A>) : Signal<'B> =
//    let t = s
//    match s with
//    | Signal(hd, tl) -> Signal(unbox(f)(hd), (delay(lazy(map f (adv tl)))))

//[<SF>]
//let rec iter (f: Box<'A -> Later<'A>>) (acc: 'A) : Signal<'A> =
//    Signal(acc, (delay(lazy(iter f (adv((unbox f) acc))))))


//[<SF>]
//let rec from () (n: int) : Signal<int> =
//    Signal(n, delay(lazy(from () (progress(n + 1)))))


let test = 
    let HEREHEHRHEHRE = fun (s: Signal<int>) -> s
    HEREHEHRHEHRE

let con_then_map = (map (box (lazy(fun (a : int) -> a * a))) (con (box (lazy(2))) ()))

let rec test4 (s: Signal<int>) x = match s with | Signal(hd, tl) -> (test4 (adv tl) x)


