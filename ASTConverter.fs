module ASTConverter

open FSharp.Compiler
open FSharp.Compiler.SyntaxTree

let private NS = "FSRP"
let private MODULE = "Core"

let private synthRange = Range.range0.MakeSynthetic()

type ASTConversionResult = 
    | Result of SynModuleOrNamespace list * string list

type IsSignalFunctionResult = 
    | Yes
    | No
    | Error of string

let private isSignalFunction isRec (binding: SynBinding) : IsSignalFunctionResult =
    match binding with
    | (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) ->
        let isSignalFunction = not attrs.IsEmpty && (List.exists (fun (a: SynAttribute) -> 
            match a.TypeName with
            | LongIdentWithDots([attrIdent], _) ->
                attrIdent.idText.Equals "SF"
        ) attrs.Head.Attributes)

        if isSignalFunction then
            if isRec then 
                if data.SynValInfo.CurriedArgInfos.Length = 2 then
                    Yes
                else
                    Error("Recursive signal function must accept exactly 2 arguments, which can be either of type unit, a single value or a tuple")
            else
                Error("Signal function must be marked recursive")
        else 
            No
                

        
    

let private getTopLevelPatternName = function
    | SynPat.Named(pat, name, _, _, _) ->
        name.idText
    | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
        String.concat "." [ for i in ident -> i.idText ]


let private makeParenExpr (innerExpr: SynExpr) = 
    SynExpr.Paren(innerExpr, synthRange, None, synthRange)


let private createSyntheticLetOrUseExpr (parentBindingsInfo: (SynValData * SynPat)) followedBy =
    let (parentArgData, parentPattern) = parentBindingsInfo
    let parentIdent = getTopLevelPatternName parentPattern
    let pattern = match parentPattern with
                    | SynPat.LongIdent(LongIdentWithDots(ident, ranges), extraIDOpt, typVarDeclsOpt, argPats, accessOpt, range) ->
                    match argPats with 
                    | SynArgPats.Pats(pats) ->
                        SynPat.LongIdent(LongIdentWithDots(ident, (List.map (fun (r: Range.range) -> r.MakeSynthetic()) ranges)), extraIDOpt, typVarDeclsOpt, SynArgPats.Pats([pats.Head]), accessOpt, range.MakeSynthetic())
    let synValData = SynValData(None, SynValInfo([parentArgData.SynValInfo.CurriedArgInfos.Head], SynArgInfo([], false, None)), None)
    let argIdentList = (List.map (fun (s: SynArgInfo) -> 
        match s.Ident with 
        | Some(ident) -> SynExpr.Ident(new Ident(ident.idText, synthRange))
        | None -> SynExpr.Const(SynConst.Unit, synthRange)
        ) parentArgData.SynValInfo.CurriedArgInfos.Head)
    let lazyAppExpr = SynExpr.Lazy(makeParenExpr(SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.Ident(new Ident(parentIdent, synthRange)), makeParenExpr(SynExpr.Tuple(false, argIdentList, [], synthRange)), synthRange)), synthRange)
    let expr = SynExpr.App(ExprAtomicFlag.Atomic, false, SynExpr.LongIdent(false, LongIdentWithDots([new Ident(NS, synthRange); new Ident(MODULE, synthRange); new Ident("delay", synthRange)], []), None, synthRange), makeParenExpr(lazyAppExpr), synthRange)
    let binding = Binding(None, SynBindingKind.NormalBinding, false, false, [], XmlDoc.PreXmlDocEmpty, synValData, pattern, None, expr, synthRange, DebugPointAtBinding(synthRange))
    SynExpr.LetOrUse(false, false, [binding], followedBy, synthRange)

let private convertTopLevelExpr expression (parentBindingsInfos: (SynValData * SynPat) list) =
    (List.fold (fun acc parentBindingInfo -> createSyntheticLetOrUseExpr parentBindingInfo acc) expression parentBindingsInfos)

let private convertDeclaration declaration =
    match declaration with
    | SynModuleDecl.Let(isRec, bindings, range) ->

        let parentBindingsInfos = List.fold (fun acc (Binding(_, _, _, _, _, _, argData, pat, _, _, _, _)) -> (argData, pat)::acc) [] bindings

        let (newBindingsAndErrors) = (List.map (fun binding ->
            match binding with
            | Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp) ->
                match isSignalFunction isRec binding with
                | Yes -> 
                    let convertedExpr = convertTopLevelExpr body parentBindingsInfos
                    let newBinding = (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, convertedExpr, m, sp))
                    (newBinding, null)
                | No -> (binding, null)
                | Error(error) -> (binding, error)
        ) bindings)

        let errors = List.filter (fun e -> not (isNull e)) (List.map snd newBindingsAndErrors)
        let newBindings = List.map fst newBindingsAndErrors
        
        (SynModuleDecl.Let(isRec, newBindings, range), errors)

    | _ -> (declaration, [])

let public convertModulesAndNamespaces modulesOrNss = 
    ASTConversionResult.Result(modulesOrNss, [])
    //ASTConversionResult.Result(List.fold (fun acc (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, access, m)) -> 
    //    let moduleDeclsAndErrors = List.map convertDeclaration decls 
    //    let moduleDecls = List.map fst moduleDeclsAndErrors
    //    let errors = List.collect (fun l -> l) (List.map snd moduleDeclsAndErrors)
    //    ((SynModuleOrNamespace(lid, isRec, isMod, moduleDecls , xml, attrs, access, m))::(fst acc), (List.append errors (snd acc)))) ([], []) modulesOrNss)
      

