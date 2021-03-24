namespace FSRP

    module Core =

        type public Later<'T> = private Later of l: Lazy<'T>

        type public Box<'T> = private Box of b: Lazy<'T>
        
        [<Struct>]
        type Signal<'T> = 
            | Signal of s: 'T * Later<Signal<'T>>

        [<Struct>]
        type Event<'T> = 
            | Event of e: Later<Event<'T>>
            | Value of v: 'T
        
        type public SFAttribute() = inherit System.Attribute()
        
        let public delay(expr: Lazy<'T>) : Later<'T> = Later(expr)
        
        let public adv(later_expr: Later<'T>) : 'T = match later_expr with | Later(lazy_expr) -> lazy_expr.Force()
        
        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(box_expr: Box<'T>) = match box_expr with | Box(box_expr) -> box_expr.Force()

        let public progress(v: 'T) = v

        let public promote(v: 'T) = v

        let rec consume_signal signal consumer countdown = 
            match signal with
            | Signal(hd, Later(tl)) -> 
                if countdown = 0 then ()
                else 
                    do consumer(hd)
                    System.GC.Collect()
                    consume_signal (tl.Force()) consumer (countdown-1)

        let rec consume_signal_inf (signal: Signal<'B>) (consumer: 'B -> _) =
            match signal with
            | Signal(hd, Later(tl)) -> 
                do consumer(hd)
                System.GC.Collect()
                consume_signal_inf (tl.Force()) consumer
        

        let executor (sf: Signal<'A> -> Signal<'B>) (input: unit -> 'A) (output: 'B -> _) =
            let count = 10000000
            let rec input_signal() = Signal((input ()), Later(lazy(input_signal())))
            let signal = sf (input_signal())
            let start= (float)System.Environment.TickCount
            let res = consume_signal_inf signal output
            let endd = (float)System.Environment.TickCount
            let elapsed = (endd - start) / 1000.
            printfn "Seconds elapsed: %f" elapsed
            printfn "Ticks per second %f" ((float)count / elapsed)

        let SF (f: 'initial -> 'now -> Later<'now -> 'ret> -> 'ret) : 'initial -> Box<'now -> 'ret> =
            let rec inner (i: 'initial) (n: 'now) : 'ret = 
                (f i n (Later(lazy(inner i))))
            
            fun (i: 'initial) -> Box(lazy(inner i))

    module Library =

        open Core

        [<SF>]
        let rec map (f: Box<'A -> 'B>) (s: Signal<'A>) : Signal<'B> = 
            match s with
            | Signal(hd, tl) -> 
                Signal(unbox(f)(hd), (delay(lazy(map f (adv tl)))))



        //let map2<'A,'B> : Box<'A -> 'B> -> Box<Signal<'A> -> Signal<'B>> = 

        //    SF(fun (f: Box<'A -> 'B>) (s: Signal<'A>) map2 ->
        //        match s with
        //        | Signal(hd, tl) -> Signal(unbox <| f <| hd, (delay(lazy(adv(map2)(adv tl)))))
        //    )

        //let iter2<'A> = 
        //    SF(fun (f: Box<'A -> Later<'A>>) (acc: 'A) iter2 ->
        //        Signal(acc, (delay(lazy(adv(iter2)(adv((unbox f) acc))))))
        //    )


        let rec iter (f: Box<'A -> Later<'A>>) (acc: 'A) : Signal<'A> =
            Signal(acc, (delay(lazy(iter f (adv((unbox f) acc))))))

        let rec con (a: Box<'A>) () : Signal<'A> = 
            Signal((unbox(a), delay(lazy(con a ()))))

        let rec from () (n: int) : Signal<int> =
            Signal(n, delay(lazy(from () (progress(n + 1)))))

        
        let map_add_1 = (map (box (lazy (fun (i: int) -> i + 1))))
        
        

        let rec ones = Signal(1, delay(lazy(ones)))



        let nats = iter (box (lazy(fun (a : int) -> delay(lazy(a + 1))))) 0


        let inner_test  (s: Signal<List<int>>) test2 : Signal<int> =
            match s with
            | Signal(hd, tl) -> Signal(List.sum hd, delay(lazy(((adv(test2)) (adv(tl))))))

        //let test2 = SF(inner_test)

        //[<SF>]
        //let rec test1 (s : Signal<List<int>>) : Signal<int> = 
        //    match s with
        //    | Signal(hd, tl) -> Signal(List.sum hd, delay(lazy(test1 (adv(tl)))))

        [<SF>]
        let rec scan (f: Box<'b -> 'a -> 'b>) (acc: 'b, i: Signal<'a>) : Signal<'b> =
            match i with 
            | Signal(a, tl) -> 
                let acc' = unbox(f) acc a
                Signal(acc', delay(lazy((scan f (acc', (adv tl))))))

        [<SF>]
        let rec zip () (sa: Signal<'a>, sb: Signal<'b>) : Signal<'a * 'b> =
            match (sa, sb) with 
            | (Signal(a, tl_a), Signal(b, tl_b)) ->
                Signal((a, b), delay(lazy( zip () (adv(tl_a), adv(tl_b)))))



        


    
