module SimplyRaTTChecker

open FSharp.Compiler.SourceCodeServices
open FSharp.Core
open FSharp.Compiler.Text
open Utility
open FSRPError

type EnvMap = Map<string,FSharpType>

type Environment = {
    InFSRPFunction: bool
    InBox: bool
    InitialEnv: EnvMap
    Now: bool
    NowEnv: EnvMap
    Later: bool
    LaterEnv: EnvMap
} 

let fsrpCoreAccessPath = "FSRP.Core"
let fsrpCoreUnstableTypeNames = ["Signal";"Event";"Later"]
let fsrpCoreStableTypeNames = ["Box"]
let fsrpCoreAttributeName = "FSRPAttribute"
let fsrpCoreSignalTypeName = "Signal"

let systemAccessPath = "System" 
let systemTypeNames = [
        "Boolean";"Byte";"SByte";"Int16";
        "UInt16";"Int32";"UInt32";"Int64";
        "UInt64";"IntPtr";"UIntPtr";"Decimal";
        "Double";"Single";"Char";"String"
    ]
let systemUnstableTypeNames = ["Lazy"]

let fsharpCoreAccessPath = "Microsoft.FSharp.Core"
let fsharpCoreTypeNames = ["Unit"]

let delayFullName = $"{fsrpCoreAccessPath}.delay"
let advFullName = $"{fsrpCoreAccessPath}.adv"
let boxFullName = $"{fsrpCoreAccessPath}.box"
let unboxFullName = $"{fsrpCoreAccessPath}.unbox"
let progressFullName = $"{fsrpCoreAccessPath}.progress"
let promoteFullName = $"{fsrpCoreAccessPath}.promote"

type CanLookupResult = 
    | Yes
    | No of FSRPError

type IsSignalFunctionResult =
    | Yes
    | YesLater
    | No
    | Error of FSRPError


let emptyMap () = Map([])


let isTypeSignal (t: FSharpType) : bool =
    t.HasTypeDefinition && t.TypeDefinition.AccessPath = fsrpCoreAccessPath && t.TypeDefinition.DisplayName = fsrpCoreSignalTypeName

let rec getReturnType(t: FSharpType) : FSharpType =
    if t.IsFunctionType then getReturnType (t.GenericArguments.Item(t.GenericArguments.Count - 1))
    else t

let isFSRPBinding (memberOrFunctionOrValue: FSharpMemberOrFunctionOrValue) : bool =
    (List.exists (fun (a : FSharpAttribute) -> 
        a.AttributeType.AccessPath = fsrpCoreAccessPath && a.AttributeType.DisplayName = fsrpCoreAttributeName
    ) (List.ofSeq memberOrFunctionOrValue.Attributes))

let isSignalFunction (memberOrFunctionOrValue: FSharpMemberOrFunctionOrValue) : IsSignalFunctionResult =

    let acceptsArgs = memberOrFunctionOrValue.CurriedParameterGroups.Count > 0

    let rec computeBodyType (ty : FSharpType) (c: int) = 
        if c > 0 then
            computeBodyType (ty.GenericArguments.Item(ty.GenericArguments.Count - 1)) (c - 1)
        else
            ty
           
    let bodyType = computeBodyType memberOrFunctionOrValue.FullType (memberOrFunctionOrValue.CurriedParameterGroups.Count)
    let bodyIsSignal = isTypeSignal bodyType 
    let bodyIsFunction = bodyType.IsFunctionType
    let bodyFunctionReturnType = getReturnType bodyType
    let bodyReturnTypeIsSignal = isTypeSignal bodyFunctionReturnType
    if bodyIsSignal then 
        if acceptsArgs then
            Yes
        else    
            Error(makeError "Signal functions directly returning a Signal must accept at least 1 argument (can be unit)" memberOrFunctionOrValue.DeclarationLocation)
    else if bodyIsFunction && bodyReturnTypeIsSignal then
        YesLater
    else 
        No

let rec isUnstableType(t: FSharpType) : bool =
    if t.IsFunctionType then true
    else if t.IsTupleType || t.IsStructTupleType || t.IsAnonRecordType then 
        isUnstableTypes (Seq.toList t.GenericArguments)
    else if t.IsAbbreviation then isUnstableType t.AbbreviatedType
    else if t.HasTypeDefinition then
        let accessPath = t.TypeDefinition.AccessPath
        let displayName = t.TypeDefinition.DisplayName
        if accessPath = fsrpCoreAccessPath then
            (List.contains displayName fsrpCoreUnstableTypeNames) || not (List.contains displayName fsrpCoreStableTypeNames)
        else if accessPath = systemAccessPath then
            (List.contains displayName systemUnstableTypeNames)
        else if t.GenericArguments.Count > 0 then
            isUnstableTypes (Seq.toList t.GenericArguments)
        else false
    else if t.IsGenericParameter then true
    else false

and isUnstableTypes (ts : FSharpType list) : bool = 
    List.fold (fun acc t -> (isUnstableType t) || acc) false ts

let rec isSignalType(t: FSharpType) : bool = 
    if t.IsFunctionType then
        let returnType = t.GenericArguments.Item(t.GenericArguments.Count-1)
        isSignalType returnType
    else if t.IsTupleType || t.IsStructTupleType || t.IsAnonRecordType then 
        List.fold (fun acc t -> (isSignalType t) || acc) false (Seq.toList t.GenericArguments)
    else if t.IsAbbreviation then isSignalType t.AbbreviatedType
    else if t.HasTypeDefinition then
        let accessPath = t.TypeDefinition.AccessPath
        let displayName = t.TypeDefinition.DisplayName
        if accessPath = fsrpCoreAccessPath && displayName = fsrpCoreSignalTypeName then
            true
        else if t.GenericArguments.Count > 0 then
           List.fold (fun acc t -> (isSignalType t) || acc) false (Seq.toList t.GenericArguments)
        else false
    else false

let isInitialEnv (env: Environment) = not env.Now && not env.Later

let isNowEnv (env: Environment) = env.Now && not env.Later

let isLaterEnv (env: Environment) = env.Now && env.Later

let addToInitialEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    { env with InitialEnv = env.InitialEnv.Add(s.LogicalName, s.FullType) }

let addToNowEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    { env with NowEnv = env.NowEnv.Add(s.LogicalName, s.FullType) }

let addToLaterEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    { env with LaterEnv = env.LaterEnv.Add(s.LogicalName, s.FullType) }

let addSymbolToCurrentEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) : Environment =
    if isLaterEnv env then
        addToLaterEnv env s
    else if isNowEnv env then
        addToNowEnv env s
    else 
        addToInitialEnv env s

let addToEnv (env: Map<string,FSharpType>) (s: FSharpMemberOrFunctionOrValue) =
    env.Add(s.LogicalName, s.FullType)

let envContains (env: Map<string,FSharpType>) (s: string) =
    match env.TryFind s with
    | Some(_) -> true
    | None -> false

let canLookup (env: Environment) (s: FSharpMemberOrFunctionOrValue) (loc: range) : CanLookupResult =
    let sName = s.LogicalName
    let sLocation = loc
    let typeIsStable = not (isUnstableType s.FullType)
    let canLookupUnstableInCurrentEnv = not env.InBox
    if isLaterEnv env then
        if envContains env.LaterEnv sName then
            if canLookupUnstableInCurrentEnv || typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access unstable variable {s.LogicalName} in current context" sLocation)
        else if envContains env.NowEnv sName then
            if typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a LATER context, as it is only accessible in a NOW context" sLocation)
        else if envContains env.InitialEnv sName then
            if typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a LATER context, as it is only accessible in an INITIAL context" sLocation)
        else CanLookupResult.Yes
            //if typeIsStable then CanLookupResult.Yes
            //else YesBut(makeWarning $"Accessing unstable variable {s.LogicalName}, which is not tracked by FSRP, may lead to space or time leaks" sLocation)
    else if isNowEnv env then
        if envContains env.LaterEnv sName then
            CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a NOW context, as it is only accessible in a LATER context" sLocation)
        else if envContains env.NowEnv sName then
            if canLookupUnstableInCurrentEnv || typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access unstable variable {s.LogicalName} in current context" sLocation)
        else if envContains env.InitialEnv sName then
            if typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a NOW context, as it is only accessible in an INITIAL context" sLocation)
        else 
            CanLookupResult.Yes
    else if isInitialEnv env then
        if envContains env.LaterEnv sName then 
            CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a INITIAL context, as it is only accessible in a LATER context" sLocation)
        else if envContains env.NowEnv sName then
                CanLookupResult.No(makeError $"Cannot access variable {s.LogicalName} in a INITIAL context, as it is only accessible in a NOW context" sLocation)
        else if envContains env.InitialEnv sName then
            if canLookupUnstableInCurrentEnv || typeIsStable then CanLookupResult.Yes
            else CanLookupResult.No(makeError $"Cannot access unstable variable {s.LogicalName} in current context" sLocation)
        else 
            CanLookupResult.Yes
    else 
        CanLookupResult.Yes


let rec checkExpr (env: Environment) (e:FSharpExpr) : FSRPError list  =
    match e with
    | BasicPatterns.AddressOf(lvalueExpr) ->
        checkExpr env lvalueExpr 
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
        appendMany [(if env.InFSRPFunction then [makeWarning "Impure expressions are discouraged in FSRP functions" e.Range] else []); (checkExpr env lvalueExpr); (checkExpr env rvalueExpr)]
    | BasicPatterns.AnonRecordGet(expr, ty, i) -> 
        checkExpr env expr 
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
        appendMany [(checkExpr env funcExpr); (checkExprs env argExprs)]
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->

        let fullName = memberOrFunc.FullName
        let exprLocation = e.Range

        let objExprErrors = checkObjArg env objExprOpt
        let (env', envErrorList) = 
                if fullName = delayFullName then
                    if isNowEnv env then  
                        ({ env with 
                            Later = true;
                        }, [])
                    else 
                        (env, [makeError $"{delayFullName} cannot appear outside a NOW context" exprLocation])
                else if fullName = advFullName then
                        if isLaterEnv env then  
                            ({ env with 
                                Later = false;
                            }, [])   
                        else 
                            (env, [makeError $"{advFullName} cannot appear outside a LATER context" exprLocation])
                else if fullName = boxFullName then
                    ({ env with 
                        InBox = true;
                        Now = true;
                        Later = false;
                    }, [])  
                //else if fullName = unboxFullName then
                    //if isNowEnv env then
                    //    printfn "%s" "ENTERING UNBOX"
                    //    ({ env with 
                    //        Now = false;
                    //        Later = false;
                    //    }, true, [])  
                    //else 
                    //    (env, true, [$"%s{formatErrorPrefix(e.Range)} {unboxFullName} cannot appear outside a NOW context"])
                else if fullName = progressFullName then 
                    if isLaterEnv env then
                        let argsAreUnstable = isUnstableTypes (List.map (fun (e: FSharpExpr) -> e.Type) argExprs)
                        let unstableArgsErrors = if argsAreUnstable then [makeError $"Arguments to {progressFullName} must be known to be stable" exprLocation] else []
                        ({ env with 
                            Later = false;
                        }, unstableArgsErrors)   
                    else 
                        (env, [makeError $"{progressFullName} cannot appear outside a LATER context" exprLocation])
                else if fullName = promoteFullName then
                    if isNowEnv env then
                        let argsAreUnstable = isUnstableTypes (List.map (fun (e: FSharpExpr) -> e.Type) argExprs)
                        let unstableArgsErrors = if argsAreUnstable then [makeError $"Arguments to {promoteFullName} must be known to be stable" exprLocation] else []
                        ({ env with 
                            Now = false
                            Later = false;
                        }, unstableArgsErrors)
                            
                    else
                        (env, [makeError $"{promoteFullName} cannot appear outside a NOW context" exprLocation])
                else 
                    (env, [])

        let lookupErrorList = 
            match canLookup env memberOrFunc e.Range with
            | CanLookupResult.No(error) -> [error]
            | CanLookupResult.Yes -> []
                                 
        appendMany [objExprErrors; envErrorList; lookupErrorList; (checkExprs env' argExprs)]
        
    | BasicPatterns.Coerce(targetType, inpExpr) ->
        checkExpr env inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
        appendMany [(checkExpr env startExpr); (checkExpr env limitExpr); (checkExpr env consumeExpr)]
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
        checkExprs env argExprs 
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
        checkObjArg env objExprOpt 
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
        appendMany [if env.InFSRPFunction then [makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range] else []; checkObjArg env objExprOpt] 
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        List.append (checkExpr env guardExpr) (List.append (checkExpr env thenExpr) (checkExpr env elseExpr)) 
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
        if isLaterEnv env && not lambdaVar.IsCompilerGenerated then
            (makeError $"Cannot declare function in a LATER context" e.Range) :: (checkExpr (addSymbolToCurrentEnv env lambdaVar) bodyExpr)
        else
            checkExpr (addSymbolToCurrentEnv env lambdaVar) bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
        List.append (checkExpr env bindingExpr) (checkExpr (addSymbolToCurrentEnv env bindingVar) bodyExpr)
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
        let recursiveBindingsMemberOrFuncOrVals = (List.map fst recursiveBindings)
        let (signalFunctionCount, errors) = 
            if env.InFSRPFunction then
                (List.fold (fun (c, ers) ((b, e): FSharpMemberOrFunctionOrValue * FSharpExpr) ->    
                    match isSignalFunction b with
                    | Yes -> (c + 1, ers)
                    | YesLater | No -> (c, ers)
                    | Error(err) -> (c, List.append [err] ers)
                ) (0, []) recursiveBindings)
            else (0, [])
        let bindingErrors = 
            if signalFunctionCount > 0 then
                if signalFunctionCount = recursiveBindings.Length then
                    let signalFunEnv = { env with Now = true; }
                    let signalFunEnvWithFunNames = List.fold addToLaterEnv signalFunEnv recursiveBindingsMemberOrFuncOrVals
                    (List.collect (fun (_, expr) -> checkExpr signalFunEnvWithFunNames expr) recursiveBindings)
                else 
                    [makeError $"Either all or no functions are signal functions in mutually recursive block" e.Range]
            else
                (List.collect (snd >> checkExpr env) recursiveBindings)
        let bodyErrors = (checkExpr env bodyExpr)
        List.append errors (List.append bindingErrors bodyErrors)
        
    | BasicPatterns.NewArray(arrayType, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.NewDelegate(delegateType, delegateBodyExpr) ->
        checkExpr env delegateBodyExpr
    | BasicPatterns.NewObject(objType, typeArgs, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.NewRecord(recordType, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.NewAnonRecord(recordType, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.NewTuple(tupleType, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.NewUnionCase(unionType, unionCase, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.Quote(quotedExpr) ->
        checkExpr env quotedExpr
    | BasicPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) ->
        checkObjArg env objExprOpt
    | BasicPatterns.AnonRecordGet(objExpr, recordOrClassType, fieldInfo) ->
        checkExpr env objExpr
    | BasicPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) ->
        appendMany [if env.InFSRPFunction then [makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range] else []; (checkObjArg env objExprOpt); (checkExpr env argExpr)]  
    | BasicPatterns.Sequential(firstExpr, secondExpr) ->
        List.append (checkExpr env firstExpr) (checkExpr env secondExpr)
    | BasicPatterns.TryFinally(bodyExpr, finalizeExpr) ->
        List.append (checkExpr env bodyExpr) (checkExpr env finalizeExpr)
    | BasicPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr) ->
        List.append (checkExpr env bodyExpr) (checkExpr env catchExpr)
    | BasicPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) ->
        checkExpr env tupleExpr
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        List.append (checkExpr env decisionExpr) (List.collect (snd >> checkExpr env) decisionTargets)
    | BasicPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) ->
        checkExprs env decisionTargetExprs
    | BasicPatterns.TypeLambda(genericParam, bodyExpr) ->
        checkExpr env bodyExpr
    | BasicPatterns.TypeTest(ty, inpExpr) ->
        checkExpr env inpExpr
    | BasicPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) ->
        appendMany [if env.InFSRPFunction then [makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range] else []; (checkExpr env unionExpr); (checkExpr env valueExpr)]
    | BasicPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) ->
        checkExpr env unionExpr
    | BasicPatterns.UnionCaseTest(unionExpr, unionType, unionCase) ->
        checkExpr env unionExpr
    | BasicPatterns.UnionCaseTag(unionExpr, unionType) ->
        checkExpr env unionExpr
    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) ->
        let baseExprErrors = checkExpr env baseCallExpr
        let memberExprErrors = List.collect (checkObjMember env) overrides
        let interfaceImplErrors = List.collect (snd >> List.collect (checkObjMember env)) interfaceImplementations
        List.append (List.append baseExprErrors memberExprErrors) interfaceImplErrors
    | BasicPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
        checkExprs env argExprs
    | BasicPatterns.ValueSet(valToSet, valueExpr) ->
        appendMany [(if env.InFSRPFunction then [makeWarning "Impure expressions are discouraged in FSRP functions" e.Range] else []); checkExpr env valueExpr]
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        List.append (checkExpr env guardExpr) (checkExpr env bodyExpr)
    | BasicPatterns.BaseValue baseType -> []
    | BasicPatterns.DefaultValue defaultType -> []
    | BasicPatterns.ThisValue thisType -> []
    | BasicPatterns.Const(constValueObj, constType) -> []
    | BasicPatterns.Value(valueToGet) -> 
        match canLookup env valueToGet e.Range with
        | CanLookupResult.No(error) -> [error]
        | CanLookupResult.Yes -> []
                
    | _ -> failwith (sprintf "unrecognized %+A" e)

and checkExprs (env: Environment) exprs : FSRPError list  =
    List.collect (fun e -> (checkExpr env e)) exprs

and checkObjArg (env: Environment) objOpt : FSRPError list  =
    doOrEmptyList objOpt (checkExpr env)

and checkObjMember (env: Environment) memb : FSRPError list  =
    checkExpr env memb.Body 

let argsToEnv (args: FSharpMemberOrFunctionOrValue list) =
    let folder = (fun (env : EnvMap) (a: FSharpMemberOrFunctionOrValue) ->
        addToEnv env a
    )
    (List.fold folder (emptyMap()) args)

let defaultEnv : Environment = 
    {
        InFSRPFunction = false;
        InBox = false;
        InitialEnv = emptyMap ()
        Now = false;
        NowEnv = emptyMap ()
        Later = false;
        LaterEnv = emptyMap ()
    }

let checkMemberOrFunctionOrValue (memberOrFuncOrValue: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) = 
    
    let isFSRPBinding = isFSRPBinding memberOrFuncOrValue
    let isSignalFunction = if isFSRPBinding then (isSignalFunction memberOrFuncOrValue) else No
    let recMemberOrFuncOrVals = 
        match mutualRecursionNamesMap.TryFind memberOrFuncOrValue.FullName with
        | Some(recNames) -> 
            let folder acc rn =
                match mutualRecursionMFVMap.TryFind rn with 
                | Some(membOrFuncOrval) -> membOrFuncOrval :: acc
                | None -> acc
            List.fold folder [] recNames
        | _ -> [] // declaration is not recursive

    let argsEnv = argsToEnv (List.collect (fun l -> l) args)   
       
    let (env, signalFuncErrors) = 
        if isFSRPBinding then 
            match isSignalFunction with 
            | Yes | No -> 
                ({ defaultEnv with  
                    InFSRPFunction = true
                    Now = true;
                    NowEnv = argsEnv;
                    LaterEnv = List.fold (fun env memberOrFunvOrVal -> addToEnv env memberOrFunvOrVal) (emptyMap ()) recMemberOrFuncOrVals
                }, [])
            | YesLater ->
                ({ defaultEnv with  
                    InFSRPFunction = true
                    InitialEnv = addToEnv argsEnv memberOrFuncOrValue
                }, [])
            | Error(error) -> (defaultEnv, [error])
        else 
            (defaultEnv, [])
    let exprError = 
        if memberOrFuncOrValue.IsCompilerGenerated then       
            try
                checkExpr env body
            with
            | _ -> []
        else checkExpr env body
    (List.append signalFuncErrors exprError)
    
let rec checkDeclarations (declarations: FSharpImplementationFileDeclaration list) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) : FSRPError list =
    (List.collect (fun d ->  checkDeclaration d mutualRecursionNamesMap mutualRecursionMFVMap) declarations)

and checkDeclaration (declaration: FSharpImplementationFileDeclaration) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) =
    match declaration with
    | FSharpImplementationFileDeclaration.Entity(entity, subDecls) -> 
        checkDeclarations subDecls mutualRecursionNamesMap mutualRecursionMFVMap
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memberOrFuncOrVal, args, body) ->
        checkMemberOrFunctionOrValue memberOrFuncOrVal args body mutualRecursionNamesMap mutualRecursionMFVMap
    | FSharpImplementationFileDeclaration.InitAction(initExpr) -> checkExpr defaultEnv initExpr

let rec bundleDeclarations (declarations: FSharpImplementationFileDeclaration list) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) : Map<string, FSharpMemberOrFunctionOrValue>  =
    (List.fold (fun acc decl ->  bundleDeclaration decl mutualRecursionNamesMap acc) mutualRecursionMFVMap declarations)

and bundleDeclaration (declaration: FSharpImplementationFileDeclaration) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>): Map<string, FSharpMemberOrFunctionOrValue>  =
    match declaration with
    | FSharpImplementationFileDeclaration.Entity(entity, subDecls) -> 
        bundleDeclarations subDecls mutualRecursionNamesMap mutualRecursionMFVMap
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(mfv, args, body) ->
        mutualRecursionMFVMap.Add(mfv.FullName, mfv)
    | FSharpImplementationFileDeclaration.InitAction(initExpr) -> mutualRecursionMFVMap

// create a map from the full name of module level let declarations to the MemberOrFunctionOrValue element that it corresponds with
let private createMutualRecursionMFVMap (implementationFileContents: FSharpImplementationFileContents) (mutualRecursionNamesMap : Map<string, string list>) =
    bundleDeclarations implementationFileContents.Declarations mutualRecursionNamesMap (Map([]))



let public checkImplementationFile (implementationFileContents: FSharpImplementationFileContents) (mutualRecursionNamesMap : Map<string, string list>) =
    let mutualRecursionMFVMap = createMutualRecursionMFVMap implementationFileContents mutualRecursionNamesMap
    checkDeclarations implementationFileContents.Declarations mutualRecursionNamesMap mutualRecursionMFVMap
    
    