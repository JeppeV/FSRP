namespace FSRP

    open System

    module Core =

        let reservedHeapIdx = 0

        module private Store =

            let mutable nowHeap = new ResizeArray<obj>()
            let mutable laterHeap = new ResizeArray<obj>()

            let progress () =
                let newLaterHeap = nowHeap
                newLaterHeap.Clear ()
                nowHeap <- laterHeap
                laterHeap <- newLaterHeap

            let cleanup () =
                nowHeap <- new ResizeArray<obj>()
                laterHeap <- new ResizeArray<obj>()

        

        type public Box<'T> = private Box of Lazy<'T>

        type public FSRPAttribute() = inherit System.Attribute()

        type public Later<'T> = private Later of int

        
        let public delay (expr: Lazy<'T>) : Later<'T> =  
            let idx = Store.laterHeap.Count
            Store.laterHeap.Add(expr)
            Later(idx)

        
        let public adv(laterExpr: Later<'T>) : 'T = 
           let (Later(idx)) = laterExpr
           let lazyExpr : Lazy<'T> = downcast Store.nowHeap.[idx]
           lazyExpr.Force()

        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(box_expr: Box<'T>) = match box_expr with | Box(box_expr) -> box_expr.Force()

        //type private Signal<'A> (hd: 'A, tlExpr: Lazy<Signal<'A>>) =
        //    let hd = hd
        //    let tlExpr = tlExpr
        //    let mutable tlDelayed = false
        //    let mutable tlLater : Later<Signal<'A>> = Later(-1)

        //    member this.Head with get() = hd
        //    member this.Tail with get() = 
        //        if not tlDelayed then
        //            tlLater <- delay tlExpr
        //            tlDelayed <- true
        //        tlLater

            

        //let Signal (hd: 'A, tlExpr: Lazy<Signal<'A>>) = new Signal<'A>(hd, tlExpr)

        //let (|Signal|) (s: Signal<'A>) = s.Head, s.Tail

        //let rec map (f: Box<'A -> 'B>) (Signal(hd, tl): Signal<'A>) =
        //    Signal (unbox f hd, lazy (map f (adv tl))) 
        
                
        [<Struct>]
        type Signal<'T> = 
            | (::) of S: 'T * Later<Signal<'T>>

        type Event<'T> = Signal<Option<'T>>

        type Eval<'IN, 'OUT> = Eval of ('IN -> ('OUT * Eval<'IN, 'OUT>))

        type Unfold<'OUT> = Unfold of (unit -> ('OUT * Unfold<'OUT>))

        type private Undefined<'IN> () =
            [<DefaultValue>] val mutable signal : Signal<'IN>

        let public buildEvaluator (sf: Signal<'IN> -> Signal<'OUT>) : Eval<'IN, 'OUT> = 

            let lazyUndefinedSignal = lazy ((new Undefined<'IN>()).signal)

            let rec restEval (is: Later<Signal<'IN>>) (os: Later<Signal<'OUT>>) (i: 'IN) =
                do Store.progress ()
                let (Later(idx)) = is
                let later = delay lazyUndefinedSignal
                Store.nowHeap.[idx] <- lazy (i :: later)
                let (o' :: os') = adv os
                (o', Eval(restEval later os'))

            let firstEval (i: 'IN) = 
                let later = delay lazyUndefinedSignal
                let inputSignal = i :: later
                let (o :: os) = sf inputSignal
                (o, Eval(restEval later os))
            
            Eval(firstEval)
            
        let public buildUnfolder (s: Signal<'OUT>) : Unfold<'OUT> =
            let rec unfold ((o :: os): Signal<'OUT>) () =
                do Store.progress ()
                let tl = adv os
                (o, Unfold(unfold tl))
            Unfold((unfold s))
            
            //let rec sf1 ((o :: os): Signal<'OUT>) = 
            //    let sf2 ((_ :: us) : Signal<unit>) =
            //        o :: delay (lazy (sf1 (adv os)) (adv us))
            //    sf2
                
            //let sf = sf1 s

            //buildEvaluator sf

        //let rec private consume_signal (signal: Signal<'B>) (output: 'B -> bool) (intervalInMillis: int) =
        //    match signal with
        //    | hd::tl -> 
        //        if output hd then
        //            do System.Threading.Thread.Sleep(intervalInMillis)
        //            do tick()
        //            do consume_signal (internal_adv tl) output intervalInMillis
        //        else 
        //            do store.cleanup()
        //            ()
                
        //[<FSRP>]
        //let private make_input_signal input_fun = 
        //    let rec input_signal () = input_fun () :: delay(lazy(input_signal ()))
        //    input_signal ()

        //let process_signal (sf: Signal<'A> -> Signal<'B>) (input: unit -> 'A) (output: 'B -> bool) (intervalInMillis: int) =
        //    let input_signal = make_input_signal input
        //    let output_signal = sf input_signal
        //    consume_signal output_signal output intervalInMillis

        //let unfold_signal (signal: Signal<'A>) (output: 'A -> bool) (intervalInMillis: int) =
        //    consume_signal signal output intervalInMillis

        //let SF (f: 'initial -> 'now -> Later<'now -> 'ret> -> 'ret) : 'initial -> Box<'now -> 'ret> =
        //    let rec inner (i: 'initial) (n: 'now) : 'ret = 
        //        (f i n (Later(lazy(inner i))))
            
        //    fun (i: 'initial) -> Box(lazy(inner i))

