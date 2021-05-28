module FSRPTests
    
    open FSRP.Core
    open FSRP.Signal
    
    module rec Inner =
        
        [<FSRP>]
        let rec test x y = mut x y

        [<FSRP>]
        let mut x y = test x y

        [<FSRP>]
        let outer f = 
            let x y = 1
            x f

        module Inner2 =
            let test2 x y = mut2 x y
            
            let mut2 x y = test2 x y

   

        
    [<FSRP>]
    let test (f: 'a -> 'b) =
        constant (box (lazy (f 1)))
    
    
    let getRandomInteger () = (new System.Random()).Next 10

    type Yes = private Yes of unit

    type Stable<'X when 'X : (static member IsStable : Yes)> = 'X

    type System.Int32 with
        static member IsStable = Yes ()        

    [<FSRP>]
    let rec doubleDelay () =
        1 :: delay (lazy ( 1 :: delay (lazy doubleDelay ())))


    [<FSRP>]
    let foo () =
        let bar (x: int) = x

        // Gets transformed into:
        let bar = fun (x: int) -> x

        ()

    [<FSRP>]
    let rec wtf () =
        1 :: delay (lazy (adv (delay (lazy (wtf ())))))

    [<FSRP>]
    let rec testRecLetInDelay i =
        let rec inner1 (x : int) = inner1 x
        i :: delay (lazy (testRecLetInDelay (inner1 i)))


    [<FSRP>]
    let rec no_tick_in_box () = 
        let t = (delay (lazy 2), delay (lazy 3))    
        1 :: delay (lazy (adv (fst t) :: delay (lazy (adv (snd t) :: (delay (lazy (no_tick_in_box ())))))))

    [<FSRP>]
    let rec stutter2 (i : int) =  
        let x = delay (lazy 1)
        i :: delay (lazy unbox (box (lazy (i :: (delay (lazy (stutter2 (i + 1))))))))
    
    let isNumberEven x = x % 2 = 0

    [<FSRP>]
    let filter (p: Box<'A -> bool>) =
        let rec fixed_point ((x::xs): Signal<'A>) : Signal<Option<'A>> =
            (if unbox p x then Some(x) else None) :: delay (lazy (fixed_point (adv xs)))
        fixed_point

    [<FSRP>]
    let filterOutOddNumbers () : Signal<int> -> Signal<Option<int>> =
        filter (box (lazy isNumberEven))

    let start () =
        let rec run (Eval(eval)) =
            let (evenNumberOpt, eval') = eval (getRandomInteger ())
            do match evenNumberOpt with
               | Some (evenNumber) -> printfn "%A is an even number!" evenNumber
               | None -> ()
            run eval'
    
        let evaluator = buildEvaluator (filterOutOddNumbers ())
        run evaluator


    let map_add_1 = (map (box (lazy (fun (i: int) -> i + 1))))
    

    [<FSRP>]
    let alt_map (f: 'A -> 'B) =
        let rec run (s : Signal<'A>) =
            (f (head s)) :: delay (lazy (run (adv (tail s))))
        run
    
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
    let app (f) (x : Later<'a>) : Later<'b> =  delay(lazy (adv f (adv x)))
    
    [<FSRP>] // different implementation of map, also using the long name for the attribute
    let rec map_patrick (f : Box<'a -> 'b>) (s : Signal<'a>) : Signal<'a> =
        match s with | x :: xs -> unbox f x :: app (delay (lazy (map_patrick f))) xs
            
    // non-FSRP function that interacts with signals
    let con_then_map c = (map (box (lazy(fun (a : int) -> a * a))) (constant (box (lazy(c)))))
    
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
        let rec inner2 () : Later<int> = 
            delay(lazy(3))
        inner2 ()

    [<FSRP>]
    let testtest () =
        let rec replaceWithX (hd::tl) =
             hd + "x" :: delay(lazy(replaceWithX (adv tl)))
        replaceWithX (constant (box(lazy("y"))))

        

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

    [<FSRP>]
    let inner_test (hd::tl) test2 : Signal<int> =
        List.sum hd :: delay(lazy(((adv(test2)) (adv(tl)))))

    type Wrapper<'T> = | Wrapper of Signal<'T>

    [<FSRP>]
    let rec test5 ((hd :: tl) : Signal<int list>) : Signal<int> = 
        List.sum hd :: delay(lazy(test5 (adv(tl))))

    
    [<FSRP>]
    let fib () = 
    
        let rec zero () = 
            0 :: delay(lazy(one ()))
        and one () =
            1 :: delay(lazy(rest 0 1))
        and rest prev1 prev2 = 
            let sum = prev1 + prev2
            sum :: delay(lazy(rest prev2 sum))
        zero ()

    [<FSRP>]
    let rec test11 (s: Signal<int>) (x : int) = match s with | hd :: tl -> hd :: delay(lazy (test11 (adv tl) x))

    [<FSRP>]
    let rec johnny (x: int) (hd::tl) : Signal<int> =
        let mutable y = 1
        y <- 2
        x::delay(lazy(johnny x (adv tl)))

    [<FSRP>]
    let rec test77 (i: Box<Later<int>>) : Signal<int> =
        1 :: delay(lazy(test77 (box (lazy(delay (lazy(1)))))))

    [<FSRP>]
    let wrapper_test () =
        Wrapper(nats ()) // is this a signal function?

    [<FSRP>]
    let rec cheatMapForLeakyNats (hd::tl) =
        do printfn "hd is %A" hd
        hd + 1 :: delay(lazy(cheatMapForLeakyNats (adv tl)))

    [<FSRP>]
    let rec leakyNats1 () =
        0 :: delay(lazy(cheatMapForLeakyNats (leakyNats1 ())))

    [<FSRP>]
    let rec leakyNats () : Signal<int> =
        0 :: delay(lazy(map (box(lazy(fun i -> i + 1))) (leakyNats ())))

    [<FSRP>]
    let rec unleakyNats () : Signal<int> =
        let maps = box (lazy (map (box (lazy (fun i -> i + 1)))))
        0 :: delay(lazy(unbox maps (leakyNats ())))

    [<FSRP>]
    let rec unstableLookup () =
        let unstable = box (lazy fun x -> x)
        box(lazy(unstable))
        
    [<FSRP>]
    let stutter () = 
        unfold 
            (box (lazy fun (count, current) ->
                if count < 0 then
                    (current, delay (lazy (count + 1, current)))
                else
                    (current + 1, delay (lazy (0, current + 1))
            ))) (0, 0)



