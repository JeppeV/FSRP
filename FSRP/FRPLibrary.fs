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

        let private store = new Store()

        type public Box<'T> = private Box of Lazy<'T>

        type public FSRPAttribute() = inherit System.Attribute()

        type public Later<'T> = private Later of int
        
        let public delay(expr: Lazy<'T>) : Later<'T> = 
            let idx = store.laterHeap.Count
            store.laterHeap.Add(expr)
            Later(idx)

        let private internal_adv(Later(idx): Later<'T>) : 'T = 
            let lazy_expr : Lazy<'T> = downcast store.nowHeap.[idx]
            lazy_expr.Force()

        let private tick () =
            do store.tick()
       
        let public adv(later_expr: Later<'T>) : 'T = 
            internal_adv(later_expr)

        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(box_expr: Box<'T>) = match box_expr with | Box(box_expr) -> box_expr.Force()

        //let public progress(v: 'T) = v

        //let public promote(v: 'T) = v

        [<Struct>]
        type Signal<'T> = 
            | (::) of s: 'T * Later<Signal<'T>>

        let rec private consume_signal_countdown signal consumer countdown = 
            match signal with
            | hd::tl -> 
                if countdown = 0 then ()
                else 
                    do consumer(hd)
                    do tick()
                    consume_signal_countdown (internal_adv tl) consumer (countdown-1)

        let rec private consume_signal_inf (signal: Signal<'B>) (consumer: 'B -> _) =
            match signal with
            | hd::tl -> 
                do consumer(hd)
                do tick()
                consume_signal_inf (internal_adv tl) consumer
        
        [<FSRP>]
        let private make_input_signal input_fun = 
            let rec input_signal () = input_fun () :: delay(lazy(input_signal ()))
            input_signal ()


        let private consume_signal (signal: Signal<'B>) (consumer: 'B -> _) =
            let count = 1000
            let start= (float)System.Environment.TickCount
            let res = consume_signal_inf signal consumer
            let endd = (float)System.Environment.TickCount
            let elapsed = (endd - start) / 1000.
            printfn "Seconds elapsed: %f" elapsed
            printfn "Ticks per second %f" ((float)count / elapsed)

        let process_signal (sf: Signal<'A> -> Signal<'B>) (input: unit -> 'A) (output: 'B -> _) =
            let input_signal = make_input_signal input
            let output_signal = sf input_signal
            consume_signal output_signal output

        let unfold_signal (signal: Signal<'A>) (output: 'A -> _) =
            consume_signal signal output

        //let SF (f: 'initial -> 'now -> Later<'now -> 'ret> -> 'ret) : 'initial -> Box<'now -> 'ret> =
        //    let rec inner (i: 'initial) (n: 'now) : 'ret = 
        //        (f i n (Later(lazy(inner i))))
            
        //    fun (i: 'initial) -> Box(lazy(inner i))

    module Library =

        open Core

        module rec Inner =
            
            [<FSRP>]
            let rec test x y = mut x y

            [<FSRP>]
            let mut x y = test x y

            [<FSRP>]
            let outer f = 
                let x y = 1
                x f

            //module Inner2 =
            //    let test2 x y = mut2 x y
                
            //    let mut2 x y = test2 x y

        [<FSRP>]
        let rec map (f: Box<'A -> 'B>) (s: Signal<'A>) : Signal<'B> = 
            match s with
            | hd :: tl -> unbox(f) (hd) :: delay(lazy(map f (adv tl)))
                

        [<FSRP>]
        let rec iter (f: Box<'A -> Later<'A>>) (acc: 'A) : Signal<'A> =
            let acc' = (unbox f) acc
            acc :: delay(lazy(iter f (adv acc')))
        
        [<FSRP>]
        let rec con (a: Box<'A>) () : Signal<'A> = 
            unbox(a) :: delay(lazy(con a ()))
        
        [<FSRP>]
        let rec from () (n: int) : Signal<int> =
            n :: delay(lazy(from () ((n + 1))))

        let map_add_1 = (map (box (lazy (fun (i: int) -> i + 1))))

        //test mutual recursion at the top level
        [<FSRP>]
        let rec direct i = 
            i :: delay(lazy(plus_one(i)))
        and [<FSRP>] plus_one i =
            let ii = i + 1
            ii :: delay(lazy(direct (ii)))

        // a function interacting with signals but not checked by FSRP
        let not_rec (inp: Signal<int>) (i: int) : Signal<int> =
            match inp with
            | _ :: tl -> i :: tl
       

        [<FSRP>] // test the auto pattern matching functionality
        let rec replace (x: int) = function
            | _ :: tl -> x :: delay(lazy(replace (x) (adv tl)))

        // FSRP function that is not interacting with signals
        [<FSRP>]
        let app (f : Later<'a -> 'b>) (x : Later<'a>) : Later<'b> =  delay(lazy (adv f (adv x)))

        [<Core.FSRP>] // different implementation of map, also using the long name for the attribute
        let rec map_patrick (f : Box<'a -> 'b>) (s : Signal<'a>) : Signal<'a> =
          match s with | x :: xs -> unbox f x :: app (delay (lazy (map_patrick f))) xs
        
        // non-FSRP function that interacts with signals
        let con_then_map c = (map (box (lazy(fun (a : int) -> a * a))) (con (box (lazy(c))) ()))

        [<FSRP>]
        let rec ones () = 1 :: delay(lazy(ones ()))

        [<FSRP>]
        let test4 x = 
            let first_inner y = 
                let rec inner41 () : Signal<int> =   
                    x :: delay(lazy(inner42 1))
                and inner42 i = 
                    y + i :: delay(lazy(inner41 ()))
                inner41 ()
            first_inner 3

        [<FSRP>]
        let test8 (x : string) = 
            let rec first_inner y = x :: delay(lazy(first_inner ((x))))
            and other_inner () = x :: delay(lazy(other_inner ()))
            first_inner

        [<FSRP>]
        let test1 y = 
            let rec inner1 () : Signal<int> = 
                y :: delay(lazy(inner1 ()))
            inner1


        [<FSRP>] // why does this one work? should it?
        let test2 () = 
            let rec inner2 () : int = 
                1 + (inner2 ())
            inner2 ()

        [<FSRP>]
        let test7 (Some(x)) =
            let rec not_sf1 i =
                let rec sf1 j =
                    j :: delay(lazy(not_sf1 j))
                and sf2 (k: string) =
                   k :: delay(lazy(sf1 k))

                sf2 i

            and not_sf2 y =
                y :: delay(lazy(not_sf1 y))

            not_sf2

        [<FSRP>]
        let rec test10 x =  
            let rec sf (y : string) =
                y::delay(lazy(sf y))
            sf


        [<FSRP>]
        let test3 () = 
            let rec inner3 () : Signal<int> = 
                1 :: delay(lazy(inner3 ()))
            inner3 ()

        let nats = iter (box (lazy(fun (a : int) -> delay(lazy((a) + 1))))) 0

        [<FSRP>]
        let inner_test (hd::tl) test2 : Signal<int> =
            List.sum hd :: delay(lazy(((adv(test2)) (adv(tl)))))

        type Wrapper<'T> = | Wrapper of Signal<'T>

        [<FSRP>]
        let rec test5 ((hd :: tl) : Signal<int list>) : Signal<int> = 
            List.sum hd :: delay(lazy(test5 (adv(tl))))

        [<FSRP>]
        let rec scan (f: Box<'b -> 'a -> Box<'b>>) (acc: Box<'b>, i: Signal<'a>) : Signal<'b> =
            match i with 
            | a :: tl ->
                let acc' = ((unbox f) (unbox acc) a)
                unbox(acc') :: delay(lazy((scan f (acc', (adv tl)))))

        [<FSRP>]
        let rec zip () (sa: Signal<'a>, sb: Signal<'b>) : Signal<'a * 'b> =
            match (sa, sb) with 
            | (a :: tl_a, b :: tl_b) ->
                (a, b) :: delay(lazy( zip () (adv(tl_a), adv(tl_b))))

        [<FSRP>]
        let rec johnny (x: int) (hd::tl) : Signal<int> =
            let mutable y = 1
            y <- 2
            x::delay(lazy(johnny x (adv tl)))

        [<FSRP>]
        let rec ruin_gc (i: int) : Signal<int> =
            let stack_frame = i + 2
            i :: delay(lazy(ruin_gc stack_frame))

        [<FSRP>]
        let wrapper_test () =
            Wrapper(nats) // is this a signal function?




        


    
