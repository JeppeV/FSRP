module FSRPTypeChecker

open FSharp.Compiler.SourceCodeServices
open FSharp.Core
open FSharp.Compiler.Text
open Utility
open FSRPError

    module private Internal =

        type EnvMap = Map<string, FSharpType * bool>

        type Environment = {
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
        let systemUnstableTypeNames = ["Lazy"]

        let delayFullName = $"{fsrpCoreAccessPath}.delay"
        let advFullName = $"{fsrpCoreAccessPath}.adv"
        let boxFullName = $"{fsrpCoreAccessPath}.box"

        type CanLookupResult = 
            | Success
            | Error of FSRPError

        let rec getReturnType(t: FSharpType) : FSharpType =
            if t.IsFunctionType then getReturnType (t.GenericArguments.Item(t.GenericArguments.Count - 1))
            else t

        let isFSRPBinding (memberOrFunctionOrValue: FSharpMemberOrFunctionOrValue) : bool =
            (List.exists (fun (a : FSharpAttribute) -> 
                a.AttributeType.AccessPath = fsrpCoreAccessPath && 
                a.AttributeType.DisplayName = fsrpCoreAttributeName
            ) (List.ofSeq memberOrFunctionOrValue.Attributes))

        let rec isUnstableType(t: FSharpType) : bool =
            if t.IsFunctionType then true
            else if t.IsTupleType || t.IsStructTupleType || t.IsAnonRecordType then 
                isUnstableTypes (Seq.toList t.GenericArguments)
            else if t.IsAbbreviation then isUnstableType t.AbbreviatedType
            else if t.HasTypeDefinition then
                let accessPath = t.TypeDefinition.AccessPath
                let displayName = t.TypeDefinition.DisplayName
                if accessPath = fsrpCoreAccessPath then
                    (List.contains displayName fsrpCoreUnstableTypeNames)// || not (List.contains displayName fsrpCoreStableTypeNames) (commented 23/05)
                else if accessPath = systemAccessPath then
                    (List.contains displayName systemUnstableTypeNames)
                else if t.GenericArguments.Count > 0 then
                    isUnstableTypes (Seq.toList t.GenericArguments)
                else false
            else if t.IsGenericParameter then true
            else false

        and isUnstableTypes (ts : FSharpType list) : bool = 
            List.fold (fun acc t -> (isUnstableType t) || acc) false ts

        let isNowEnv (env: Environment) = not env.Later

        let isLaterEnv (env: Environment) = env.Later

        let addToNowEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
            { env with NowEnv = env.NowEnv.Add(s.LogicalName, (s.FullType, true)) }

        let addToLaterEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) =
            { env with LaterEnv = env.LaterEnv.Add(s.LogicalName, (s.FullType, true)) }

        let addSymbolToCurrentEnv (env: Environment) (s: FSharpMemberOrFunctionOrValue) : Environment =
            if isLaterEnv env then
                addToLaterEnv env s
            else addToNowEnv env s

        let addToEnv (env: EnvMap) (s: FSharpMemberOrFunctionOrValue) =
            env.Add(s.LogicalName, (s.FullType, true))

        let stabilizeEnv (envMap: EnvMap) : EnvMap =
            Map.fold (fun acc name (fsharpType, _) -> 
                (acc.Add (name, (fsharpType, (not (isUnstableType fsharpType)))))      
            ) (Map([])) envMap

        let hideUnstableVariables (env: Environment) : Environment =
            { env with NowEnv = stabilizeEnv env.NowEnv; LaterEnv = stabilizeEnv env.LaterEnv }
        
        let tryLookup (env: Environment) (var: FSharpMemberOrFunctionOrValue) (varLoc: range) : CanLookupResult =
            let varName = var.LogicalName
            let typeIsStable = not (isUnstableType var.FullType)
            let mutable lookupRef : FSharpType * bool = (var.FullType, true)
            if isLaterEnv env then
                if env.LaterEnv.TryGetValue (varName, &lookupRef) then
                    let (_, isVisible) = lookupRef
                    if isVisible then 
                        Success
                    else 
                        Error(makeError $"Cannot access unstable variable {var.LogicalName} in current context" varLoc)
                else if env.NowEnv.TryGetValue (varName, &lookupRef) then
                    let (_, isVisible) = lookupRef
                    if isVisible && typeIsStable then 
                        Success
                    else 
                        Error(makeError $"Cannot access variable {var.LogicalName} in a LATER context, as it is only accessible in a NOW context" varLoc)
                else Success
            else
                if env.LaterEnv.TryGetValue (varName, &lookupRef) then
                    Error(makeError $"Cannot access variable {var.LogicalName} in a NOW context, as it is only accessible in a LATER context" varLoc)
                else if env.NowEnv.TryGetValue (varName, &lookupRef) then
                    let (_, isVisible) = lookupRef
                    if isVisible then 
                        Success
                    else 
                        Error(makeError $"Cannot access unstable variable {var.LogicalName} in current context" varLoc)
                else 
                    Success

        let rec checkExpr (env: Environment) (e:FSharpExpr) : FSRPError list =
            match e with
            | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
                List.append (checkExpr env bindingExpr) (checkExpr (addSymbolToCurrentEnv env bindingVar) bodyExpr)   
            
            | BasicPatterns.Call(objExprOpt, funcVar, _, _, argExprs) ->
                let funcFullName = funcVar.FullName
                let funcVarLocation = e.Range
                let lookupErrors = 
                    match tryLookup env funcVar funcVarLocation with
                    | Error(error) -> [error]
                    | _ -> []

                let objExprErrors = checkObjArg env objExprOpt
                let (updatedEnv, envErrors) = 
                        if funcFullName = delayFullName then
                            if isNowEnv env then
                                ({ env with 
                                    Later = true;
                                }, [])
                            else 
                                (env, [makeError $"{delayFullName} cannot appear outside a NOW context" funcVarLocation])
                        else if funcFullName = advFullName then
                                if isLaterEnv env then  
                                    ({ env with 
                                        Later = false;
                                    }, [])   
                                else 
                                    (env, [makeError $"{advFullName} cannot appear outside a LATER context" funcVarLocation])
                        else if funcFullName = boxFullName then
                            let stableNowEnv = hideUnstableVariables { env with Later = false; }
                            (stableNowEnv, [])
                        else 
                            (env, [])
                let argErrors = checkExprs updatedEnv argExprs              
                appendMany [objExprErrors; envErrors; lookupErrors; argErrors] 
            
            | BasicPatterns.LetRec(recursiveBindings, bodyExpr) ->
                let letRecVars = (List.map fst recursiveBindings)
                let letRecBodies = (List.map snd recursiveBindings)
                let bindingErrors = 
                    let stableEnv = hideUnstableVariables env
                    let stableEnvWithRecLetVarsInLater = List.fold addToLaterEnv stableEnv letRecVars
                    (List.collect (checkExpr stableEnvWithRecLetVarsInLater) letRecBodies)
                let envWithRecLetVarsInNow = List.fold addToNowEnv env letRecVars
                let bodyErrors = (checkExpr envWithRecLetVarsInNow bodyExpr)
                List.append bindingErrors bodyErrors

            | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
                if isLaterEnv env && not lambdaVar.IsCompilerGenerated then
                    (makeError $"Cannot declare function in a LATER context" e.Range) :: 
                    (checkExpr (addSymbolToCurrentEnv env lambdaVar) bodyExpr)
                else
                    checkExpr (addSymbolToCurrentEnv env lambdaVar) bodyExpr

            | BasicPatterns.AddressOf(lvalueExpr) ->
                checkExpr env lvalueExpr 
            | BasicPatterns.AddressSet(lvalueExpr, rvalueExpr) ->
                appendMany [[makeWarning "Impure expressions are discouraged in FSRP functions" e.Range]; (checkExpr env lvalueExpr); (checkExpr env rvalueExpr)]
            | BasicPatterns.AnonRecordGet(expr, ty, i) -> 
                checkExpr env expr 
            | BasicPatterns.Application(funcExpr, typeArgs, argExprs) ->
                appendMany [(checkExpr env funcExpr); (checkExprs env argExprs)]
            | BasicPatterns.Coerce(targetType, inpExpr) ->
                checkExpr env inpExpr
            | BasicPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp) ->
                appendMany [(checkExpr env startExpr); (checkExpr env limitExpr); (checkExpr env consumeExpr)]
            | BasicPatterns.ILAsm(asmCode, typeArgs, argExprs) ->
                checkExprs env argExprs 
            | BasicPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) ->
                checkObjArg env objExprOpt 
            | BasicPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) ->
                appendMany [[makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range]; checkObjArg env objExprOpt] 
            | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
                List.append (checkExpr env guardExpr) (List.append (checkExpr env thenExpr) (checkExpr env elseExpr)) 
            | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->
                List.append (checkExpr env bindingExpr) (checkExpr (addSymbolToCurrentEnv env bindingVar) bodyExpr)        
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
                appendMany [[makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range]; (checkObjArg env objExprOpt); (checkExpr env argExpr)]  
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
                appendMany [[makeWarning "Impure expressions are discouraged in FSRP bindings" e.Range]; (checkExpr env unionExpr); (checkExpr env valueExpr)]
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
                appendMany [[makeWarning "Impure expressions are discouraged in FSRP functions" e.Range]; checkExpr env valueExpr]
            | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
                List.append (checkExpr env guardExpr) (checkExpr env bodyExpr)
            | BasicPatterns.BaseValue baseType -> []
            | BasicPatterns.DefaultValue defaultType -> []
            | BasicPatterns.ThisValue thisType -> []
            | BasicPatterns.Const(constValueObj, constType) -> []
            | BasicPatterns.Value(valueToGet) -> 
                match tryLookup env valueToGet e.Range with
                | Error(error) -> [error]
                | _ -> []
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
            (List.fold folder (Map([])) args)

        let checkMemberOrFunctionOrValue (memberOrFuncOrValue: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) = 
    
            let isFSRPBinding = isFSRPBinding memberOrFuncOrValue

            if isFSRPBinding && args.Length = 0 then [(makeError "FSRP bindings must accept at least 1 argument (can be unit)" memberOrFuncOrValue.DeclarationLocation)]
            else if isFSRPBinding then
                let recursionVariables = 
                    match mutualRecursionNamesMap.TryFind memberOrFuncOrValue.FullName with
                    | Some(recNames) -> 
                        let folder acc rn =
                            match mutualRecursionMFVMap.TryFind rn with 
                            | Some(membOrFuncOrval) -> membOrFuncOrval :: acc
                            | None -> acc
                        List.fold folder [] recNames
                    | _ -> [] // declaration is not recursive

                let args = argsToEnv (List.collect (fun l -> l) args)   
       
                let env = 
                    {   
                        NowEnv = args;
                        Later = false;
                        LaterEnv = List.fold 
                                    (fun laterEnv recVar -> addToEnv laterEnv recVar) 
                                    (Map([])) 
                                    recursionVariables
                    }
                    
                let exprError = 
                    if memberOrFuncOrValue.IsCompilerGenerated then       
                        try
                            checkExpr env body
                        with
                        | _ -> []
                    else checkExpr env body
                exprError
            else
                []
    
        let rec checkDeclarations (declarations: FSharpImplementationFileDeclaration list) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) : FSRPError list =
            (List.collect (fun d ->  checkDeclaration d mutualRecursionNamesMap mutualRecursionMFVMap) declarations)

        and checkDeclaration (declaration: FSharpImplementationFileDeclaration) (mutualRecursionNamesMap : Map<string, string list>) (mutualRecursionMFVMap: Map<string, FSharpMemberOrFunctionOrValue>) =
            match declaration with
            | FSharpImplementationFileDeclaration.Entity(entity, subDecls) -> 
                checkDeclarations subDecls mutualRecursionNamesMap mutualRecursionMFVMap
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memberOrFuncOrVal, args, body) ->
                checkMemberOrFunctionOrValue memberOrFuncOrVal args body mutualRecursionNamesMap mutualRecursionMFVMap
            | FSharpImplementationFileDeclaration.InitAction(initExpr) -> []

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
        let createMutualRecursionMFVMap (implementationFileContents: FSharpImplementationFileContents) (mutualRecursionNamesMap : Map<string, string list>) =
            bundleDeclarations implementationFileContents.Declarations mutualRecursionNamesMap (Map([]))

let public checkImplementationFile (implementationFileContents: FSharpImplementationFileContents) (mutualRecursionNamesMap : Map<string, string list>) =
    let mutualRecursionMFVMap = Internal.createMutualRecursionMFVMap implementationFileContents mutualRecursionNamesMap
    Internal.checkDeclarations implementationFileContents.Declarations mutualRecursionNamesMap mutualRecursionMFVMap
    
    