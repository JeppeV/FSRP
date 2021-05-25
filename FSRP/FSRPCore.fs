namespace FSRP

    open System

    module Core =

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

        
        type public FSRPAttribute() = inherit System.Attribute()

        type public Box<'T> = private Box of Lazy<'T>

        type public Later<'T> = private Later of int

        type public Signal<'T> = | (::) of 'T * Later<Signal<'T>>

        type public Event<'T> = Signal<Option<'T>>
        
        let public delay (expr: Lazy<'T>) : Later<'T> =  
            let idx = Store.laterHeap.Count
            Store.laterHeap.Add(expr)
            Later(idx)

        let public adv(laterExpr: Later<'T>) : 'T = 
           let (Later(idx)) = laterExpr
           let lazyExpr : Lazy<'T> = downcast Store.nowHeap.[idx]
           lazyExpr.Force()

        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(Box(boxExpr): Box<'T>) = boxExpr.Force()

        type Eval<'IN, 'OUT> = Eval of ('IN -> ('OUT * Eval<'IN, 'OUT>))

        type Unfold<'OUT> = Unfold of (unit -> ('OUT * Unfold<'OUT>))

        type private Undefined<'IN> () =
            [<DefaultValue>] val mutable signal : Signal<'IN>

        let public resetStore () = Store.cleanup ()

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


// FOR DISCUSSION, DON'T DELETE

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
