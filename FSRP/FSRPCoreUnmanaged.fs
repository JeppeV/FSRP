namespace FSRP

    open System

    module CoreUnmanaged =

        type public FSRPAttribute() = inherit System.Attribute()

        type public Box<'T> = private Box of Lazy<'T>
        
        type public Later<'T> = private Later of Lazy<'T>

        type public Signal<'T> = 
            | (::) of S: 'T * Later<Signal<'T>>

        type public Event<'T> = Signal<Option<'T>>
        
        let public delay (expr: Lazy<'T>) : Later<'T> =  
            Later(expr)

        let public adv(Later(lazyExpr): Later<'T>) : 'T = 
            lazyExpr.Force()

        let public box(expr: Lazy<'T>) : Box<'T> = Box(expr)
        
        let public unbox(box_expr: Box<'T>) = match box_expr with | Box(box_expr) -> box_expr.Force()        
                
        type Eval<'IN, 'OUT> = Eval of ('IN -> ('OUT * Eval<'IN, 'OUT>))

        type Unfold<'OUT> = Unfold of (unit -> ('OUT * Unfold<'OUT>))

        type private Undefined<'IN> () =
            [<DefaultValue>] val mutable signal : Signal<'IN>

        let public buildEvaluator (sf: Signal<'IN> -> Signal<'OUT>) : Eval<'IN, 'OUT> = 

            let rec restEval (tailRef: Signal<'IN> ref) (os: Later<Signal<'OUT>>) (i: 'IN) =
                let newTailRef = ref (new Undefined<'IN>()).signal
                tailRef := i :: delay (lazy (!newTailRef))
                let (o' :: os') = adv os
                (o', Eval(restEval newTailRef os'))

            let firstEval (i: 'IN) = 
                printfn "Starting with NO managed store"
                let tailRef = ref (new Undefined<'IN>()).signal
                let inputSignal = i :: delay (lazy (!tailRef))
                let (o :: os) = sf inputSignal
                (o, Eval(restEval tailRef os))
            
            Eval(firstEval)
                    
        let public buildUnfolder (s: Signal<'OUT>) : Unfold<'OUT> =
            let rec unfold ((o :: os): Signal<'OUT>) () =
                let tl = adv os
                (o, Unfold(unfold tl))
            Unfold((unfold s))
