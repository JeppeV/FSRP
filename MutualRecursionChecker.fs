module PurityChecker

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Utility

let rec last (l: 'a list) : Option<'a> =
    match l with
    | hd::[] -> Some(hd)
    | [] -> None
    | _::tl -> last tl



//let rec checkPattern p : string list = 
//    match p with
//      | SynPat.Wild(_) ->
//          printfn "  .. underscore pattern"; []
//      | SynPat.Named(pat, name, _, _, _) ->
//          checkPattern pat
//          printfn "  .. named as '%s'" name.idText; []
//          //printfn "ident: %A" name
//      | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
//          let names = String.concat "." [ for i in ident -> i.idText ]
//          printfn "  .. identifier: %s" names; []
//      | pat -> printfn "  .. other pattern: %A" pat; []
//      | SynPat.Ands(pats, range) -> []
//      | SynPat.ArrayOrList(isArray, elementPats, range) ->[]
//      | SynPat.Attrib(pat, attributes, range) -> []
//      | SynPat.Const(constant, range) -> []
//      | SynPat.DeprecatedCharRange(startChar, endChar, range) -> []
//      | SynPat.FromParseError(pat, range) -> []
//      | SynPat.InstanceMember(thisId, memberId, toolingIdOpt, accessibility, range) -> []
//      | SynPat.IsInst(synType, range) -> []
//      | SynPat.LongIdent(LongIdentWithDots(ident, ranges), extraId, typarDecls, argPats, accessbility, range) -> []
//      | SynPat.Named(pat, ident, isSelfIdentifier, accessibility, range) -> []
//      | SynPat.OptionalVal(ident, range) -> []
//      | SynPat.Or(lhsPat, rhsPat, range) -> []
//      | SynPat.Paren(pat, range) -> []
//      | SynPat.QuoteExpr(expr, range) -> [] // EXPR HERE
//      | SynPat.Record(fieldPats, range) -> []
//      | SynPat.Tuple(isStruct, elementPats, range) -> []
//      | SynPat.Typed(pat, targetType, range) -> []
//      | SynPat.Wild(range) -> []

//and checkPatterns (ps: SynPat list) =
//    (List.collect checkPattern ps)

//let rec checkExpression e : string list =
//    match e with
//    | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
//        let condExprErrors = checkExpression cond
//        let trueExprErrors = checkExpression trueBranch
//        let falseExprErrors = doOrEmptyList falseBranchOpt checkExpression

//        appendMany([condExprErrors; trueExprErrors; falseExprErrors])
//    | SynExpr.LetOrUse(isRec, _, bindings, body, _) ->
//        // Visit bindings (there may be multiple
//        // for 'let .. = .. and .. = .. in ...'
//        if (isRec) then printfn "Let expr is rec"
//        let bindingErrors = 
//            List.collect (fun (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) -> 
//                appendMany([checkPattern pat; checkExpression body])
//            ) bindings

//        let bodyErrors = checkExpression body
//        appendMany([bindingErrors; bodyErrors])
//    | expr -> printfn " - expression: %A" expr; [] // REMOVE
//    | SynExpr.AnonRecd(isStruct, copyInfoOpt, recordFields, range) -> 
//        let copyInfoErrors : string list = doOrEmptyList copyInfoOpt (fun (expr, _) -> (checkExpression expr))
//        let recordFieldsErrors = List.collect (fun (_, expr) -> checkExpression expr) recordFields
//        appendMany([copyInfoErrors; recordFieldsErrors])
//    | SynExpr.App(atomicFlag, isInfix, funcExpr, argExpr, range) -> 
//        checkExpressions [funcExpr; argExpr]
//    | SynExpr.ArrayOrList(isList, exprs, range) -> checkExpressions exprs
//    | SynExpr.ArrayOrListOfSeqExpr(isArray, expr, range) -> checkExpression expr
//    | SynExpr.Const(constant, range) -> []
//    | SynExpr.Do(expr, range) -> checkExpression expr
//    | SynExpr.DotGet(expr, rangeOfDot, longDotId, range) -> checkExpression expr
//    | SynExpr.DotIndexedGet(objectExpr, indexArgs, dotRange, range) -> 
//        let indexExprs = (List.collect (fun (ia: SynIndexerArg) -> ia.Exprs) indexArgs)
//        checkExpressions (objectExpr::indexExprs)
//    | SynExpr.Downcast(expr, targetType, range) -> 
//        checkExpression expr
//    | SynExpr.Ident(ident) -> []
//    | SynExpr.LongIdent(isOptional, longDotId, altNameRefCellOpt, range) -> []
//    | SynExpr.InferredDowncast(expr, range) -> checkExpression expr
//    | SynExpr.InferredUpcast(expr, range) -> checkExpression expr
//    | SynExpr.InterpolatedString(contents, range) -> 
//        let exprs = 
//            (List.fold (fun (acc: SynExpr list)  (isp: SynInterpolatedStringPart) ->
//                match isp with
//                | SynInterpolatedStringPart.FillExpr(expr, _) -> expr :: acc
//                | _ -> acc

//            ) [] contents)
//        checkExpressions exprs
//    | SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, parsedDataOpt, range) -> 
//        let bodyErrors = checkExpression body
//        let parseDataErrors = doOrEmptyList parsedDataOpt (fun (synPatList, expr) -> appendMany([checkPatterns synPatList; checkExpression expr]))
//        appendMany [bodyErrors; parseDataErrors]
//    | SynExpr.Lazy(expr, range) -> checkExpression expr
//    | SynExpr.Match(matchSeqPoint, expr, clauses, range) -> 
//        let clausesErrors =
//            (List.collect (fun (Clause(pat, whenExprOpt, resultExpr, range, sp)) -> 
//                  appendMany [checkPattern pat; checkExpression resultExpr; doOrEmptyList whenExprOpt checkExpression]
//            ) clauses)
//        appendMany [checkExpression expr; clausesErrors]
//    | SynExpr.MatchLambda(isExnMatch, keywordRange, clauses, matchSeqPoint, range) -> 
//        let clausesErrors =
//            (List.collect (fun (Clause(pat, whenExprOpt, resultExpr, range, sp)) -> 
//                  appendMany [checkPattern pat; checkExpression resultExpr; doOrEmptyList whenExprOpt checkExpression]
//            ) clauses)
//        clausesErrors
//    | SynExpr.New(isProtected, targetType, expr, range) -> checkExpression expr
//    | SynExpr.Null(range) -> []
//    | SynExpr.ObjExpr(objType, argOptionsOpt, bindings, extraImpls, newExprRange, range) -> 
        
//        let argOptionsErrors = doOrEmptyList argOptionsOpt (fun (expr, _) -> checkExpression expr)
//        let bindingErrors = checkBindingExpressions bindings
//        let interfaceImplErrorsList = 
//            List.collect (fun (InterfaceImpl(ty, bindings, range)) -> (checkBindingExpressions bindings)) extraImpls
//        appendMany [argOptionsErrors; bindingErrors; interfaceImplErrorsList]
//    | SynExpr.Paren(expr, leftParenRange, rightParenRangeOpt, range) -> checkExpression expr
//    | SynExpr.Quote(operator, isRaw, quotedExpr, isFromQueryExpression, range) -> checkExpressions [operator; quotedExpr]
//    | SynExpr.Record(baseInfoOpt, copyInfoOpt, recordFields, range) -> 
//        let baseInfoErrors = doOrEmptyList baseInfoOpt (fun (_, expr, _, _, _) -> checkExpression expr)
//        let copyInfoErrors = doOrEmptyList copyInfoOpt (fun (expr, _) -> checkExpression expr)
//        let recordFieldErrors = 
//            List.collect (fun (_, exprOpt, _) -> doOrEmptyList exprOpt checkExpression) recordFields
//        appendMany([baseInfoErrors; copyInfoErrors; recordFieldErrors])
//    | SynExpr.TraitCall(supportTys, traitSig, argExpr, range) -> []
//    | SynExpr.TryFinally(tryExpr, finallyExpr, range, trySeqDebug, finallySeqDebug) -> []
//    | SynExpr.TryWith(tryExpr, tryRange, withCases, withRange, range, trySeqPoint, withSeqPoint) -> []
//    | SynExpr.Tuple(isStruct, exprs, commaRanges, range) -> []
//    | SynExpr.TypeApp(expr, lessRange, typeArgs, commaRanges, greaterRangeOpt, typeArgsRange, range) -> []
//    | SynExpr.Typed(expr, targetType, range) -> []
//    | SynExpr.TypeTest(expr, targetType, range) -> []
//    | SynExpr.Upcast(expr, targetType, range) -> []

//    (* POTENTIALLY IMPURE EXPRESSIONS  *)
//    | SynExpr.AddressOf(isByref, expr, opRange, range) -> [] // allow?
//    | SynExpr.Assert(expr, range) -> [] // allow?
//    | SynExpr.For(forSeqPoint, ident, identBody, direction, toBody, doBody, range) -> [] // allow?
//    | SynExpr.DotSet(targetExpr, longDotId, rhsExpr, range) -> [] // allow?
//    | SynExpr.DotIndexedSet(objectExpr, indexExprs, valueExpr, leftOfSetRange, dotRange, range) -> [] // allow?
//    | SynExpr.Fixed(expr, range) -> [] // allow?
//    | SynExpr.DotNamedIndexedPropertySet(targetExpr, longDotId, argExpr, rhsExpr, range) -> [] // allow?
//    | SynExpr.NamedIndexedPropertySet(longDotId, expr1, expr2, range) -> [] // allow?
//    | SynExpr.Sequential(seqPoint, isTrueSeq, expr1, expr2, range) -> [] // allow?
//    | SynExpr.Set(targetExpr, rhsExpr, range) -> [] // allow?
//    | SynExpr.While(whileSeqPoint, whileExpr, doExpr, range) -> [] // allow?
//    | SynExpr.ForEach(forSeqPoint, seqExprOnly, isFromSource, pattern, enumExpr, bodyExpr, range) -> [] // allow?


//    (* COMPUTATION EXPRESSIONS  *)
//    | SynExpr.CompExpr(isArrayOrList, isNotNakedRefCell, expr, range) -> [] // allow?
//    | SynExpr.JoinIn(lhsExpr, lhsRange, rhsExpr, range) -> [] // allow?
//    | SynExpr.DoBang(expr, range) -> [] // allow?
//    | SynExpr.LetOrUseBang(bindSeqPoint, isUse, isFromSource, pattern, rhsExpr, andBangs, body, range) -> [] // allow?
//    | SynExpr.MatchBang(matchSeqPoint, expr, clauses, range) -> [] // allow?
//    | SynExpr.LongIdentSet(longDotId, expr, range) -> [] // allow?
//    | SynExpr.YieldOrReturn(flags, expr, range) -> []
//    | SynExpr.YieldOrReturnFrom(flags, expr, range) -> []

//    (* INTERNAL AST EXPRESSIONS *)
//    | SynExpr.LibraryOnlyILAssembly(ilCode, typeArgs, args, retTy, range) -> []
//    | SynExpr.LibraryOnlyStaticOptimization(constraints, expr, optimizedExpr, range) -> []
//    | SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, fieldNum, range) -> []
//    | SynExpr.LibraryOnlyUnionCaseFieldSet(expr, longId, fieldNum, rhsExpr, range) -> []
//    | SynExpr.SequentialOrImplicitYield(seqPoint, expr1, expr2, ifNotStmt, range) -> []
//    | SynExpr.ImplicitZero(range) -> [] // parse error
//    | SynExpr.ArbitraryAfterError(debugStr, range) -> [] // parse error
//    | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, range) -> [] // parse error
//    | SynExpr.FromParseError(expr, range) -> []

//and checkExpressions (exprs: SynExpr list) =
//    List.collect (fun e -> checkExpression e) exprs

//and checkBindingExpressions (bindings: SynBinding list) =
//    List.collect (fun (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) -> 
//        appendMany([checkPattern pat; checkExpression body])
//    ) bindings



let longIdentToString (lident: LongIdent) : string =
    String.concat "." (List.map (fun (ident: Ident) -> ident.idText) lident)
    

let isFSRPFunction (attrs: SynAttributes) =
    (List.exists (fun (attrsList: SynAttributeList) -> 
        (List.exists (fun (attr: SynAttribute) -> 
            match attr.TypeName with   
            | LongIdentWithDots(idents, _) -> 
                match (last idents) with 
                | Some(ident) -> ident.idText = "FSRP"
                | None -> failwith "No identifier for attribute"
        ) attrsList.Attributes)
    ) attrs)
  
let getFunNameFromPattern (pat: SynPat) = 
    match pat with 
    | SynPat.Named(pat, name, _, _, _) ->
        name.idText
    | SynPat.LongIdent(LongIdentWithDots(idents, _), _ , _, _, _, _) ->
       (String.concat "." (Seq.map (fun (i: Ident) -> i.idText) idents))
        
    | _ -> failwith $"Recursive function binding has pattern %A{pat}.   (handle it)"

let collectAllBindingNamesAndFSRPCount (decls: SynModuleDecl list) =
    
    List.fold (fun (declsAcc, declsFsrpCount) decl -> 
        match decl with
        | SynModuleDecl.Let(isRec, bindings, range) ->
            let (bindingsNames, fsrpCount) = 
                List.fold (fun (bindingsAcc, fsrpCount) binding ->
                    let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
                    ((getFunNameFromPattern pat)::bindingsAcc, if isFSRPFunction attrs then fsrpCount + 1 else fsrpCount)
                ) ([], 0) bindings
            (List.append bindingsNames declsAcc, fsrpCount + declsFsrpCount)
        | _ -> (declsAcc, declsFsrpCount)
    ) ([], 0) decls



let rec checkModuleOrNamespaceDeclarations decls currentModuleName isRecModule moduleRange =


    let (allDeclsRecFunNames, fullFsrpCount) = 
        if isRecModule then 
            collectAllBindingNamesAndFSRPCount decls
        else ([], -1) // this is never used

    let declarationFolder ((mutualRecursionNamesMap, errors) : Map<string, string list> * string list) (declaration:  SynModuleDecl) =
        match declaration with
        | SynModuleDecl.Let(isRec, bindings, range) ->
            // Let binding as a declaration is similar to let binding
            // as an expression (in checkExpression), but has no body

            let bindingFolder ((recFunNames, fsrpCount) : string list * int) (binding:  SynBinding) =
                let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
                let isFSRPFunction = isFSRPFunction attrs
                let recFunNameOpt = 
                    if isRec || isRecModule(* && isFSRPFunction*) then // collect info for all recursive declarations
                        Some(getFunNameFromPattern pat)
                    else None

                let newFsrpCount =  if isFSRPFunction then fsrpCount + 1 else fsrpCount
                
                let newRecFunNames =
                    match recFunNameOpt with
                    | Some(recFunName) -> (currentModuleName + "." + recFunName :: recFunNames)
                    | None -> recFunNames

                (newRecFunNames, newFsrpCount)
            
            let (recFunNames, fsrpCount) = (List.fold bindingFolder ([], 0) bindings)

            let mutRecErrors = 
                if (not isRecModule) && (fsrpCount > 0 && fsrpCount <> bindings.Length) then
                    [$"%A{(formatErrorPrefix range)} Either all or no let bindings in mutual recusion block must have FSRP attribute"]
                else []

            let moreMutualRecursionNamesMap = 
                let res = 
                    if isRecModule then                        
                        List.fold (fun (acc : Map<string,string list>) (recFunName : string) -> acc.Add(recFunName, allDeclsRecFunNames)) (Map([])) recFunNames
                    else if isRec then 
                        List.fold (fun (acc : Map<string,string list>) (recFunName : string) -> acc.Add(recFunName, recFunNames)) (Map([])) recFunNames
                    else 
                        Map([])
                res

            (joinMaps mutualRecursionNamesMap moreMutualRecursionNamesMap, List.append mutRecErrors errors)
        
        | SynModuleDecl.NestedModule(ComponentInfo(attrs, _, _, lident, _, _, _, _), isRecursive, decls, isContinuing, range) -> 
            let (moreMutualRecursionNamesMap, errors') = checkModuleOrNamespaceDeclarations decls $"%s{currentModuleName}.%s{(longIdentToString (lident))}" (isRecursive || isRecModule) range
            (joinMaps mutualRecursionNamesMap moreMutualRecursionNamesMap, List.append errors' errors)
        | _ -> (mutualRecursionNamesMap, errors)
        
        // potentially need to handle these cases, idk yet
        //| SynModuleDecl.Attributes(attributes, range) -> ()
        //| SynModuleDecl.DoExpr(spInfo, expr, range) -> ()
        //| SynModuleDecl.Exception(exnDefn, range) -> ()
        //| SynModuleDecl.HashDirective(hashDirective, range) -> ()
        //| SynModuleDecl.ModuleAbbrev(ident, longIdent, range) -> ()
        //| SynModuleDecl.NamespaceFragment(fragment) -> ()
        //| SynModuleDecl.Open(target, range) -> ()
        //| SynModuleDecl.Types(typeDefns, range) -> ()
        
    let recModuleErrors = 
        if isRecModule && (fullFsrpCount > 0) && (fullFsrpCount <> allDeclsRecFunNames.Length) then
            [$"%A{(formatErrorPrefix moduleRange)} Either all or no let bindings in recursive module must have FSRP attribute"]
        else []

    let (mutRecInfo, errors) = List.fold declarationFolder (Map([]), []) decls
    (mutRecInfo, appendMany [errors; recModuleErrors])


let checkModulesOrNamespaces (modulesOrNss : SynModuleOrNamespace list) =
    let folder ((mutualRecursionNamesMaps, errors) : Map<string, string list> * string list) (modOrNs:  SynModuleOrNamespace) =
        let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, moduleRange)) = modOrNs
        let (mutualRecursionNamesMaps', errors') = checkModuleOrNamespaceDeclarations decls (longIdentToString lid) isRec moduleRange
        (joinMaps mutualRecursionNamesMaps' mutualRecursionNamesMaps, List.append errors' errors)

    let res = List.fold folder (Map([]), []) modulesOrNss
    res
       

let topLevelMutualRecursionChecker (parseFileResults: FSharpParseFileResults) =
    match parseFileResults.ParseTree with
    | Some(parsedInput) -> 
        match parsedInput with
        | ParsedInput.ImplFile(parsedImplFileInput) ->
            let (ParsedImplFileInput(fn, script, name, scopedPragmas, hashDirectives, modules, isLastCompiland)) = parsedImplFileInput
            checkModulesOrNamespaces modules
        | ParsedInput.SigFile(parsedSigFileInput) -> (Map([]), []) // signature files do not contain expressions (i think)
    | _ -> failwith $"No parse tree available for file {parseFileResults.FileName}"

  