namespace FSRP

    open System

    module Core =

        type private Store() =
            member val nowHeap = new ResizeArray<obj>() with get, set
            member val laterHeap = new ResizeArray<obj>() with get, set
            member this.tick () =
                let newLaterHeap = this.nowHeap
                newLaterHeap.Clear()
                this.nowHeap <- this.laterHeap
                this.laterHeap <- newLaterHeap
            member this.cleanup () =
                this.nowHeap <- new ResizeArray<obj>()
                this.laterHeap <- new ResizeArray<obj>()

        let private store = new Store()

        type public Box<'T> = private Box of Lazy<'T>

        type public FSRPAttribute() = inherit System.Attribute()

        type public Later<'T> = private Later of int
        
        let public delay(expr: Lazy<'T>) : Later<'T> =  
            let idx = store.laterHeap.Count
            store.laterHeap.Add(expr)
            let res : Later<'T> = Later(idx)
            printfn "ADD IDX %A, TYPE: %A" idx (res.GetType ())
            res

        let private internal_adv(later: Later<'T>) : 'T = 
            let (Later(idx)) = later
            let c = store.nowHeap.[idx]
            printfn "GET IDX %A, TYPE: %A" idx (later.GetType ())
            let lazy_expr : Lazy<'T> = downcast c
            let result = lazy_expr.Force()
            
            result

        let private tick () =
            printfn "TICK"
            do store.tick()
       
        let public adv(later_expr: Later<'T>) : 'T = 
            internal_adv(later_expr)

        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(box_expr: Box<'T>) = match box_expr with | Box(box_expr) -> box_expr.Force()

        [<Struct>]
        type Signal<'T> = 
            | (::) of S: 'T * Later<Signal<'T>>

        type Event<'T> = Signal<Option<'T>>

        let rec private consume_signal (signal: Signal<'B>) (output: 'B -> bool) (intervalInMillis: int) =
            match signal with
            | hd::tl -> 
                if output hd then
                    do System.Threading.Thread.Sleep(intervalInMillis)
                    do tick()
                    do consume_signal (internal_adv tl) output intervalInMillis
                else 
                    do store.cleanup()
                    ()
                
        [<FSRP>]
        let private make_input_signal input_fun = 
            let rec input_signal () = input_fun () :: delay(lazy(input_signal ()))
            input_signal ()

        let process_signal (sf: Signal<'A> -> Signal<'B>) (input: unit -> 'A) (output: 'B -> bool) (intervalInMillis: int) =
            let input_signal = make_input_signal input
            let output_signal = sf input_signal
            consume_signal output_signal output intervalInMillis

        let unfold_signal (signal: Signal<'A>) (output: 'A -> bool) (intervalInMillis: int) =
            consume_signal signal output intervalInMillis

        //let SF (f: 'initial -> 'now -> Later<'now -> 'ret> -> 'ret) : 'initial -> Box<'now -> 'ret> =
        //    let rec inner (i: 'initial) (n: 'now) : 'ret = 
        //        (f i n (Later(lazy(inner i))))
            
        //    fun (i: 'initial) -> Box(lazy(inner i))

    module Library =

        open Core

        [<FSRP>]
        let rec map (f: Box<'X -> 'Y>) ((x :: xs): Signal<'X>) : Signal<'Y> = 
            unbox f x :: delay (lazy (map f (adv xs)))

        [<FSRP>]
        let rec iter (f: Box<'X -> Later<'X>>) (acc: 'X) : Signal<'X> =
            let acc' = (unbox f) acc
            acc :: delay (lazy (iter f (adv acc')))
                
        [<FSRP>]
        let rec from (i: int) : Signal<int> =
            i :: delay (lazy (from (i + 1)))

        [<FSRP>]
        let rec constant (x: Box<'X>) =
            unbox x :: delay (lazy (constant x))

        [<FSRP>]
        let rec filter (p: Box<'X -> bool>) ((x::xs): Signal<'X>) : Signal<Option<'X>> =
            (if unbox p x then Some(x) else None) :: delay (lazy (filter p (adv xs)))

        [<FSRP>]
        let rec scan (f: Box<Box<'Y> -> 'X -> Box<'Y>>) (acc: Box<'Y>) ((x :: xs): Signal<'X>) : Signal<Box<'Y>> =
            let acc' = ((unbox f) acc x)
            acc' :: delay (lazy (scan f acc' (adv xs)))

        [<FSRP>]
        let rec zipWith (f: Box<'X -> 'Y -> 'Z>) ((x::xs): Signal<'X>) ((y::ys): Signal<'Y>) : Signal<'Z> =
            unbox f x y :: delay (lazy (zipWith f (adv xs) (adv ys)))

        [<FSRP>]
        let rec zip (xs: Signal<'X>, ys: Signal<'Y>) : Signal<'X * 'Y> =
            zipWith (box (lazy (fun x y -> (x, y)))) xs ys

        [<FSRP>]
        let rec switch ((x :: xs) : Signal<'X>) ((xOpt :: xOpts) : Event<Signal<'X>>) : Signal<'X> =
            match xOpt with 
            | Some(x' :: xs') -> x' :: delay (lazy (switch (adv xs') (adv xOpts)))
            | None -> x :: delay (lazy (switch (adv xs) (adv xOpts)))

        let tail (_ :: tl) = tl

        let head (hd :: _) = hd

        [<FSRP>]
        let switchFun (sf: Signal<'X> -> Signal<'Y>) (e: Event<Signal<'X> -> Signal<'Y>>) (xs: Signal<'X>) : Signal<'Y> =
            let rec switchFun' ((y :: ys): Signal<'Y>) ((e :: es): Event<Signal<'X> -> Signal<'Y>>) (xs: Signal<'X>) : Signal<'Y> =
                match e with
                | None -> 
                    y :: delay (lazy (switchFun' (adv ys) (adv es) (adv (tail xs))))
                | Some(sf') -> 
                    let (y' :: ys') = sf' xs
                    y' :: delay (lazy (switchFun' (adv ys') (adv es) (adv (tail xs))))
            switchFun' (sf xs) e xs
                

        let zeros = constant (box (lazy 0))

        let nats = iter (box (lazy (fun i -> delay (lazy (i + 1))))) 0



        


    
