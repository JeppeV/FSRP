module SimplyRaTTChecker

open FSharp.Compiler.SourceCodeServices
open FSharp.Core
open FSharp.Compiler.Range

type SignalFunInfo = {
    SignalFunName: string
    IsRecApp: bool
}

type Environment = {
    SignalFunInfo: SignalFunInfo
    InitialEnv: Set<string>
    InitialEnvCG: Set<string>
    Now: bool
    NowEnv: Set<string>
    NowEnvCG: Set<string>
    Later: bool
    LaterEnv: Set<string>
    LaterEnvCG: Set<string>
} 


let formatErrorPrefix (r: range) : string =
    $"%s{r.FileName} %A{r.Start}-%A{r.End} FSRP error"

let fsrpCoreAccessPath = "FSRP.Core"
let fsrpCoreUnstableTypeNames = ["Signal";"Event";"Later"]
let fsrpCoreStableTypeNames = ["Box"]

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
    else if t.IsTupleType || t.IsStructTupleType || t.IsAnonRecordType || t.IsFunctionType then 
        List.fold (fun acc t -> (isSignalType t) || acc) false (Seq.toList t.GenericArguments)
    else if t.IsAbbreviation then isSignalType t.AbbreviatedType
    else if t.HasTypeDefinition then
        let accessPath = t.TypeDefinition.AccessPath
        let displayName = t.TypeDefinition.DisplayName
        if accessPath = fsrpCoreAccessPath && displayName = "Signal" then
            true
        else if t.GenericArguments.Count > 0 then
           List.fold (fun acc t -> (isSignalType t) || acc) false (Seq.toList t.GenericArguments)
        else false
    else false

let isSignalFunctionType(t: FSharpType) : bool = 
    t.IsFunctionType && (isSignalType (t.GenericArguments.Item (t.GenericArguments.Count-1)))


let isInitialEnv (env: Environment) = not env.Now && not env.Later

let isNowEnv (env: Environment) = env.Now && not env.Later

let isLaterEnv (env: Environment) = env.Now && env.Later

let addToEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) : Environment =
    if isLaterEnv env then
        { env with LaterEnv = env.LaterEnv.Add(s.LogicalName) }
    else if isNowEnv env then
        { env with NowEnv = env.NowEnv.Add(s.LogicalName) }
    else 
        { env with InitialEnv = env.InitialEnv.Add(s.LogicalName) }
        
type CanLookupResult = 
    | Yes
    | No of string

let addToInitialEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    { env with InitialEnv = env.InitialEnv.Add(s.LogicalName) }

let addToNowEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    { env with NowEnv = env.NowEnv.Add(s.LogicalName) }

let isInInitialEnvCG (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    env.InitialEnvCG.Contains(s.LogicalName)

let isInNowEnvCG (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    env.NowEnvCG.Contains(s.LogicalName)

let canLookup (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
    if env.SignalFunInfo.IsRecApp then Yes else // If we are looking up variables in a recursive application of the function, anything goes
        if isLaterEnv env then
            if env.LaterEnv.Contains(s.LogicalName) then
                Yes
            else if env.NowEnv.Contains(s.LogicalName) then
                No(($"%s{formatErrorPrefix(s.DeclarationLocation)}Cannot access variable {s.LogicalName} in a LATER context, as it is only accessible in a NOW context"))
            else if env.InitialEnv.Contains(s.LogicalName) then
                No($"%s{formatErrorPrefix(s.DeclarationLocation)}Cannot access variable {s.LogicalName} in a LATER context, as it is only accessible in an INITIAL context")
            else Yes
        else if isNowEnv env then
            if env.LaterEnv.Contains(s.LogicalName) then
                No($"%s{formatErrorPrefix(s.DeclarationLocation)} Cannot access variable {s.LogicalName} in a NOW context, as it is only accessible in a LATER context")
            else if env.NowEnv.Contains(s.LogicalName) then
                Yes
            else if env.InitialEnv.Contains(s.LogicalName) then
                No($"%s{formatErrorPrefix(s.DeclarationLocation)} Cannot access variable {s.LogicalName} in a NOW context, as it is only accessible in an INITIAL context")
            else Yes
        else if isInitialEnv env then
            if env.LaterEnv.Contains(s.LogicalName) then 
                No($"%s{formatErrorPrefix(s.DeclarationLocation)} Cannot access variable {s.LogicalName} in a INITIAL context, as it is only accessible in a LATER context")
            else if env.NowEnv.Contains(s.LogicalName) then
                 No($"%s{formatErrorPrefix(s.DeclarationLocation)} Cannot access variable {s.LogicalName} in a INITIAL context, as it is only accessible in a NOW context")
            else if env.InitialEnv.Contains(s.LogicalName) then
                Yes
            else Yes
        else Yes


let rec checkExpr (env: Environment) (e:FSharpExpr) : string list  =

    //printfn "%A" e
    match e with
    | BasicPatterns.AddressOf(lvalueExpr) ->
        checkExpr env lvalueExpr 
    | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
        List.append (checkExpr env lvalueExpr) (checkExpr env rvalueExpr) 
    | BasicPatterns.AnonRecordGet(expr, ty, i) -> 
        checkExpr env expr 
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
        //printfn "APP FUNC EXP: %A" funcExpr
        //printfn "APP ARG EXPS: %A" argExprs
        match funcExpr with 
        | BasicPatterns.Value(value) when value.LogicalName.Equals env.SignalFunInfo.SignalFunName ->
            match canLookup env value with
            | Yes ->    
                let funInfo = { env.SignalFunInfo with IsRecApp = true }
                checkExprs { env with SignalFunInfo = funInfo } argExprs
            | No(error) -> [error]
        | _ -> List.append (checkExpr env funcExpr) (checkExprs env argExprs)

    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->

        let fullName = memberOrFunc.FullName

        let funInfo = 
            if memberOrFunc.LogicalName.Equals env.SignalFunInfo.SignalFunName then
                { env.SignalFunInfo with IsRecApp = true }
            else 
                env.SignalFunInfo           
        
        let objExprErrors = checkObjArg env objExprOpt
        let (env', isTemporal, envErrorList) = 
                if memberOrFunc.LogicalName = env.SignalFunInfo.SignalFunName then
                    let errors = if isLaterEnv env then [] else [$"%s{formatErrorPrefix(e.Range)} Recursive application cannot appear outside a LATER context"]
                    ({ env with SignalFunInfo = { env.SignalFunInfo with IsRecApp = true }}, false, errors)
                else if fullName = delayFullName then
                    if isNowEnv env then  
                        printfn "%s" "ENTERING DELAY"
                        ({ env with 
                            Later = true;
                        }, true, [])
                    else 
                        (env, true, [$"%s{formatErrorPrefix(e.Range)} {delayFullName} cannot appear outside a NOW context"])
                else if fullName = advFullName then
                        if isLaterEnv env then  
                            printfn "%s" "ENTERING ADV"
                            ({ env with 
                                Later = false;
                            }, true, [])   
                        else 
                            (env, true, [$"%s{formatErrorPrefix(e.Range)} {advFullName} cannot appear outside a LATER context"])
                else if fullName = boxFullName then
                    if isInitialEnv env then
                        printfn "%s" "ENTERING BOX"
                        ({ env with 
                            Now = true;
                            Later = false;
                        }, true, [])  
                    else 
                        (env, true, [$"%s{formatErrorPrefix(e.Range)} {boxFullName} cannot appear outside an INITIAL context"])
                //else if fullName = unboxFullName then
                //    if isNowEnv env then
                //        printfn "%s" "ENTERING UNBOX"
                //        ({ env with 
                //            Now = false;
                //            Later = false;
                //        }, true, [])  
                //    else 
                //        (env, true, [$"%s{formatErrorPrefix(e.Range)} {unboxFullName} cannot appear outside a NOW context"])
                else if fullName = progressFullName then 
                    if isLaterEnv env then
                        let argsAreUnstable = isUnstableTypes (List.map (fun (e: FSharpExpr) -> e.Type) argExprs)
                        if argsAreUnstable then 
                            (env, true, [$"%s{formatErrorPrefix(e.Range)} arguments to {progressFullName} must be stable"])
                        else 
                            printfn "%s" "ENTERING PROGRESS"
                            ({ env with 
                                Later = false;
                            }, true, [])   
                    else 
                        (env, true, [$"%s{formatErrorPrefix(e.Range)} {progressFullName} cannot appear outside a LATER context"])
                else if fullName = promoteFullName then
                    if isNowEnv env then
                        let argsAreUnstable = isUnstableTypes (List.map (fun (e: FSharpExpr) -> e.Type) argExprs)
                        if argsAreUnstable then
                            (env, true, [$"%s{formatErrorPrefix(e.Range)} arguments to {promoteFullName} must be stable"])
                        else 
                            printfn "%s" "ENTERING PROMOTE"
                            ({ env with 
                                Now = false
                                Later = false;
                            }, true, [])   
                    else
                        (env, true, [$"%s{formatErrorPrefix(e.Range)} {promoteFullName} cannot appear outside a NOW context"])
                else 
                    (env, false, [])

        let lookupErrorList = if not isTemporal then 
                                match canLookup env memberOrFunc with
                                | No(error) -> [error]
                                | Yes -> []
                               else []
        
        //printfn "------ CALLING %s WITH ARGS %A"  memberOrFunc.LogicalName argExprs
        List.append objExprErrors (List.append envErrorList (List.append lookupErrorList (checkExprs { env' with SignalFunInfo = funInfo } argExprs)))         
        
    | BasicPatterns.Coerce(targetType, inpExpr) ->
        checkExpr env inpExpr
    | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
        List.append (List.append (checkExpr env startExpr) (checkExpr env limitExpr)) (checkExpr env consumeExpr)
    | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
        checkExprs env argExprs 
    | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
        checkObjArg env objExprOpt 
    | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
        checkObjArg env objExprOpt 
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        List.append (checkExpr env guardExpr) (List.append (checkExpr env thenExpr) (checkExpr env elseExpr)) 
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
        let b = bodyExpr
        if isLaterEnv env && not lambdaVar.IsCompilerGenerated then
            [$"%s{formatErrorPrefix(lambdaVar.DeclarationLocation)} Cannot declare function in a LATER context"]
        else
            checkExpr env bodyExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
        //printfn "LET BindingVar %A" bindingVar
        //printfn "LET BindingExpr %A" bindingExpr
        let env' = 
            match bindingExpr with  
            | BasicPatterns.UnionCaseGet(BasicPatterns.Value(valueToGet), unionType, unionCase, unionCaseField) when valueToGet.IsCompilerGenerated ->
                // compiler generated let expression for pattern matched arguments
                if isInInitialEnvCG env valueToGet then
                    addToInitialEnv env bindingVar
                else if isInNowEnvCG env valueToGet then
                    addToNowEnv env bindingVar
                else failwith $"Compiler generated let expression for name %A{bindingVar} does not exist in CG environments"
            | _ -> addToEnv env bindingVar

        printfn "LET %A: Expr of type %A is signal: %A " bindingVar bindingExpr.Type (isSignalType bindingExpr.Type)
        List.append (checkExpr env bindingExpr) (checkExpr env' bodyExpr)
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
        List.append (List.collect (snd >> checkExpr env) recursiveBindings) (checkExpr env bodyExpr)
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
        List.append (checkObjArg env objExprOpt) (checkExpr env argExpr)
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
        List.append (checkExpr env unionExpr) (checkExpr env valueExpr)
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
        checkExpr env valueExpr
    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        List.append (checkExpr env guardExpr) (checkExpr env bodyExpr)
    | BasicPatterns.BaseValue baseType -> []
    | BasicPatterns.DefaultValue defaultType -> []
    | BasicPatterns.ThisValue thisType -> []
    | BasicPatterns.Const(constValueObj, constType) -> []
    | BasicPatterns.Value(valueToGet) -> 
        match canLookup env valueToGet with
        | No(error) -> [error]
        | Yes -> []
    | _ -> failwith (sprintf "unrecognized %+A" e)

and checkExprs (env: Environment) exprs : string list  =
    List.collect (fun e -> (checkExpr env e)) exprs

and checkObjArg (env: Environment) objOpt : string list  =
    match objOpt with
    | Some(expr) -> checkExpr env expr
    | None -> []

and checkObjMember (env: Environment) memb : string list  =
    checkExpr env memb.Body 

let argsToEnvs (args: FSharpMemberOrFunctionOrValue list) =
    let folder = (fun ((args, argsCG) : Set<string> * Set<string>) (a: FSharpMemberOrFunctionOrValue) ->
        if a.IsCompilerGenerated then
            (args, argsCG.Add(a.FullName))
        else 
            (args.Add(a.FullName), argsCG)
    )
    (List.fold folder (Set([]), Set([])) args)


let isSignalFunction (memberOrFunctionOrValue: FSharpMemberOrFunctionOrValue) =
    (List.exists (fun (a : FSharpAttribute) -> 
        a.AttributeType.AccessPath = fsrpCoreAccessPath && a.AttributeType.DisplayName = "SFAttribute"
    ) (List.ofSeq memberOrFunctionOrValue.Attributes))

let checkMemberOrFunctionOrValue (memberOrFuncOrValue: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) = 
    
    let isSignalFunction = isSignalFunctionType memberOrFuncOrValue.FullType
    printfn "Checking declaration %A, isSignalFunction: %A" memberOrFuncOrValue.LogicalName isSignalFunction
    //printfn "With body: %A" body

    

    let defaultEnv : Environment = 
                {
                    SignalFunInfo = {
                        SignalFunName = ""
                        IsRecApp = false;
                    };
                    InitialEnv = Set([]);
                    InitialEnvCG = Set([]);
                    Now = false;
                    NowEnv = Set([]);
                    NowEnvCG = Set([]);
                    Later = false;
                    LaterEnv = Set([]);
                    LaterEnvCG = Set([])
                }

    let env : Environment = 
        if args.Length > 0 then
            let (argsEnv, argsEnvCG) = argsToEnvs (List.collect (fun l -> l) args)     
            if isSignalFunction then
                { defaultEnv with  
                    SignalFunInfo = {
                        SignalFunName = memberOrFuncOrValue.LogicalName
                        IsRecApp = false;
                    };
                    Now = true;
                    NowEnv = argsEnv;
                    NowEnvCG = argsEnvCG;
                    LaterEnv = Set([memberOrFuncOrValue.LogicalName]); 
                }
            else 
                { defaultEnv with  
                    SignalFunInfo = {
                        SignalFunName = ""
                        IsRecApp = false;
                    };
                    InitialEnv = argsEnv.Add(memberOrFuncOrValue.LogicalName)
                    InitialEnvCG = argsEnvCG
                }
        else 
            defaultEnv
    if memberOrFuncOrValue.IsCompilerGenerated then       
        try
            checkExpr env body
        with
        | _ -> []
    else checkExpr env body
    


let rec checkDeclarations (declarations: FSharpImplementationFileDeclaration list) : string list =
    (List.collect checkDeclaration declarations)

and checkDeclaration (declaration: FSharpImplementationFileDeclaration) =
    match declaration with
    | FSharpImplementationFileDeclaration.Entity(entity, subDecls) -> 
        checkDeclarations subDecls
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memberOrFuncOrVal, args, body) ->
        let m = memberOrFuncOrVal
        let a = args
        let b = body
        checkMemberOrFunctionOrValue memberOrFuncOrVal args body
    | FSharpImplementationFileDeclaration.InitAction(initExpr) -> []

let public checkImplementationFile (implementationFileContents: FSharpImplementationFileContents) =
    checkDeclarations implementationFileContents.Declarations
    
    