namespace FSRP

    module Signal =

        open Core

        [<FSRP>]
        let inline tail (_ :: tl) = tl

        [<FSRP>]
        let inline head (hd :: _) = hd

        [<FSRP>]
        let rec map (f: Box<'X -> 'Y>) ((x :: xs): Signal<'X>) : Signal<'Y> = 
            unbox f x :: delay (lazy (map f (adv xs)))

        [<FSRP>]
        let rec iter (f: Box<'State -> Later<'State>>) (state: 'State) : Signal<'State> =
            let state' = (unbox f) state
            state :: delay (lazy (iter f (adv state')))
                
        [<FSRP>]
        let rec from (n: int) : Signal<int> = iter (box (lazy (fun i -> delay (lazy (i + 1))))) n

        [<FSRP>]
        let rec constant (x: Box<'X>) =
            unbox x :: delay (lazy (constant x))

        [<FSRP>]
        let rec alternate ((x1::x1s): Signal<'X>) ((_::x2s) : Signal<'X>) =
            x1 :: delay (lazy (alternate' (adv x1s) (adv x2s)))
        
        and [<FSRP>] private alternate' ((_::x1s): Signal<'X>) ((x2::x2s): Signal<'X>) =
            x2 :: delay (lazy (alternate (adv x1s) (adv x2s)))

        [<FSRP>]
        let rec filter (p: Box<'X -> bool>) ((x::xs): Signal<'X>) : Signal<Option<'X>> =
            (if unbox p x then Some(x) else None) :: delay (lazy (filter p (adv xs)))

        [<FSRP>]
        let rec scan (f: Box<Box<'State> -> 'X -> Box<'State>>) (state: Box<'State>) ((x :: xs): Signal<'X>) : Signal<'State> =
            let state' = ((unbox f) state x)
            unbox state' :: delay (lazy (scan f state' (adv xs)))

        [<FSRP>]
        let rec zipWith (f: Box<'X -> 'Y -> 'Z>) ((x::xs): Signal<'X>) ((y::ys): Signal<'Y>) : Signal<'Z> =
            unbox f x y :: delay (lazy (zipWith f (adv xs) (adv ys)))

        [<FSRP>]
        let inline zip (xs: Signal<'X>, ys: Signal<'Y>) : Signal<'X * 'Y> =
            zipWith (box (lazy (fun x y -> (x, y)))) xs ys

        [<FSRP>]
        let rec switch ((x :: xs) : Signal<'X>) ((xOpt :: xOpts) : Event<Signal<'X>>) : Signal<'X> =
            match xOpt with 
            | Some(x' :: xs') -> x' :: delay (lazy (switch (adv xs') (adv xOpts)))
            | None -> x :: delay (lazy (switch (adv xs) (adv xOpts)))

        [<FSRP>]
        let inline switchFun (sf: Signal<'X> -> Signal<'Y>) (e: Event<Signal<'X> -> Signal<'Y>>) (xs: Signal<'X>) : Signal<'Y> =
            let rec switchFun' ((y :: ys): Signal<'Y>) ((e :: es): Event<Signal<'X> -> Signal<'Y>>) (xs: Signal<'X>) : Signal<'Y> =
                match e with
                | None -> 
                    y :: delay (lazy (switchFun' (adv ys) (adv es) (adv (tail xs))))
                | Some(sf') -> 
                    let (y' :: ys') = sf' xs
                    y' :: delay (lazy (switchFun' (adv ys') (adv es) (adv (tail xs))))
            switchFun' (sf xs) e xs

        [<FSRP>]
        let zeros () : Signal<int> = constant (box (lazy 0))

        [<FSRP>]
        let ones () : Signal<int> = constant (box (lazy 1))

        [<FSRP>]
        let nats () = iter (box (lazy (fun i -> delay (lazy (i + 1))))) 0

        [<FSRP>]
        let rec unfold (f: Box<'State -> ('Y * Later<'State>)>) (state: 'State) : Signal<'Y> = 
            let (x, state') = unbox f state
            x :: delay (lazy (unfold f (adv state')))






        


    
