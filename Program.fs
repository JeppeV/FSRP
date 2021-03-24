module Program

open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler

open Fantomas

// Create an interactive checker instance
let checker = FSharpChecker.Create(keepAssemblyContents=true)

let printList (l: 'T list) =
    List.iter (fun o -> printfn "%A" o) l
  

/// Walk over a pattern - this is for example used in
/// let <pat> = <expr> or in the 'match' expression
let rec visitPattern = function
  | SynPat.Wild(_) ->
      printfn "  .. underscore pattern"
  | SynPat.Named(pat, name, _, _, _) ->
      visitPattern pat
      printfn "  .. named as '%s'" name.idText
      //printfn "ident: %A" name
  | SynPat.LongIdent(LongIdentWithDots(ident, _), _, _, _, _, _) ->
      let names = String.concat "." [ for i in ident -> i.idText ]
      printfn "  .. identifier: %s" names
  | pat -> printfn "  .. other pattern: %A" pat
  | SynPat.Ands(pats, range) -> ()
  | SynPat.ArrayOrList(isArray, elementPats, range) -> ()
  | SynPat.Attrib(pat, attributes, range) -> ()
  | SynPat.Const(constant, range) -> ()
  | SynPat.DeprecatedCharRange(startChar, endChar, range) -> ()
  | SynPat.FromParseError(pat, range) -> ()
  | SynPat.InstanceMember(thisId, memberId, toolingIdOpt, accessibility, range) -> ()
  | SynPat.IsInst(synType, range) -> ()
  | SynPat.LongIdent(LongIdentWithDots(ident, ranges), extraId, typarDecls, argPats, accessbility, range) -> ()
  | SynPat.Named(pat, ident, isSelfIdentifier, accessibility, range) -> ()
  | SynPat.Null(range) -> ()
  | SynPat.OptionalVal(ident, range) -> ()
  | SynPat.Or(lhsPat, rhsPat, range) -> ()
  | SynPat.Paren(pat, range) -> ()
  | SynPat.QuoteExpr(expr, range) -> () // EXPR HERE
  | SynPat.Record(fieldPats, range) -> ()
  | SynPat.Tuple(isStruct, elementPats, range) -> ()
  | SynPat.Typed(pat, targetType, range) -> ()
  | SynPat.Wild(range) -> ()
/// Walk over an expression - if expression contains two or three
/// sub-expressions (two if the 'else' branch is missing), let expression
/// contains pattern and two sub-expressions

let rec checkExpression = function
  | SynExpr.IfThenElse(cond, trueBranch, falseBranchOpt, _, _, _, _) ->
      // Visit all sub-expressions
      printfn "Conditional:"
      checkExpression cond
      checkExpression trueBranch
      falseBranchOpt |> Option.iter checkExpression
  | SynExpr.LetOrUse(isRec, _, bindings, body, _) ->
      // Visit bindings (there may be multiple
      // for 'let .. = .. and .. = .. in ...'
      if (isRec) then printfn "Let expr is rec"
      printfn "LetOrUse with the following bindings:"
      for binding in bindings do
        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc,
                     data, pat, retInfo, init, m, sp)) = binding
        //printfn "%s : %A" "Binding pattern" pat 
        visitPattern pat
        printfn "Child Binding Pat: %A" pat
        printfn "Child Args: %A" data    
        printfn "Child ginding expr: %A" init
        
        //checkExpression init
        //printfn "%s : %A" "Binding expr" init 
      // Visit the body expression
      printfn "And the following body:"
      checkExpression body
  | expr -> printfn " - expression: %A" expr
  | SynExpr.AnonRecd(isStruct, copyInfoOpt, recordFields, range) -> ()
  | SynExpr.App(atomicFlag, isInfix, funcExpr, argExpr, range) -> ()
  | SynExpr.ArrayOrList(isList, exprs, range) -> ()
  | SynExpr.ArrayOrListOfSeqExpr(isArray, expr, range) -> ()
  | SynExpr.Const(constant, range) -> ()
  | SynExpr.Do(expr, range) -> ()
  | SynExpr.DotGet(expr, rangeOfDot, longDotId, range) -> ()
  | SynExpr.DotIndexedGet(objectExpr, indexExprs, dotRange, range) -> ()  
  | SynExpr.Downcast(expr, targetType, range) -> () 
  | SynExpr.Ident(ident) -> ()
  | SynExpr.LongIdent(isOptional, longDotId, altNameRefCellOpt, range) -> ()
  | SynExpr.IfThenElse(ifExpr, thenExpr, elseExprOpt, splfToThen, isFromErrorRecovery, ifToThenRange, range) -> ()
  | SynExpr.InferredDowncast(expr, range) -> ()
  | SynExpr.InferredUpcast(expr, range) -> ()
  | SynExpr.InterpolatedString(contents, range) -> ()
  | SynExpr.Lambda(fromMethod, inLambdaSeq, args, body, parsedDataOpt, range) -> ()
  | SynExpr.Lazy(expr, range) -> ()
  | SynExpr.LetOrUse(isRecursive, isUse, bindings, body, range) -> ()
  | SynExpr.Match(matchSeqPoint, expr, clauses, range) -> ()
  | SynExpr.MatchLambda(isExnMatch, keywordRange, matchClauses, matchSeqPoint, range) -> ()
  | SynExpr.New(isProtected, targetType, expr, range) -> ()
  | SynExpr.Null(range) -> ()
  | SynExpr.ObjExpr(objType, argOptionsOpt, bindings, extraImpls, newExprRange, range) -> ()
  | SynExpr.Paren(expr, leftParenRange, rightParenRangeOpt, range) -> ()
  | SynExpr.Quote(operator, isRaw, quotedExpr, isFromQueryExpression, range) -> ()
  | SynExpr.Record(baseInfoOpt, copyInfoOpt, recordFields, range) -> ()
  | SynExpr.TraitCall(supportTys, traitSig, argExpr, range) -> ()
  | SynExpr.TryFinally(tryExpr, finallyExpr, range, trySeqDebug, finallySeqDebug) -> ()
  | SynExpr.TryWith(tryExpr, tryRange, withCases, withRange, range, trySeqPoint, withSeqPoint) -> ()
  | SynExpr.Tuple(isStruct, exprs, commaRanges, range) -> ()
  | SynExpr.TypeApp(expr, lessRange, typeArgs, commaRanges, greaterRangeOpt, typeArgsRange, range) -> ()
  | SynExpr.Typed(expr, targetType, range) -> ()
  | SynExpr.TypeTest(expr, targetType, range) -> ()
  | SynExpr.Upcast(expr, targetType, range) -> ()
  
  (* POTENTIALLY IMPURE EXPRESSIONS  *)
  | SynExpr.AddressOf(isByref, expr, opRange, range) -> () // allow?
  | SynExpr.Assert(expr, range) -> () // allow?
  | SynExpr.For(forSeqPoint, ident, identBody, direction, toBody, doBody, range) -> () // allow?
  | SynExpr.DotSet(targetExpr, longDotId, rhsExpr, range) -> () // allow?
  | SynExpr.DotIndexedSet(objectExpr, indexExprs, valueExpr, leftOfSetRange, dotRange, range) -> () // allow?
  | SynExpr.Fixed(expr, range) -> () // allow?
  | SynExpr.DotNamedIndexedPropertySet(targetExpr, longDotId, argExpr, rhsExpr, range) -> () // allow?
  | SynExpr.NamedIndexedPropertySet(longDotId, expr1, expr2, range) -> () // allow?
  | SynExpr.Sequential(seqPoint, isTrueSeq, expr1, expr2, range) -> () // allow?
  | SynExpr.Set(targetExpr, rhsExpr, range) -> () // allow?
  | SynExpr.While(whileSeqPoint, whileExpr, doExpr, range) -> () // allow?
  | SynExpr.ForEach(forSeqPoint, seqExprOnly, isFromSource, pattern, enumExpr, bodyExpr, range) -> () // allow?


  (* COMPUTATION EXPRESSIONS  *)
  | SynExpr.CompExpr(isArrayOrList, isNotNakedRefCell, expr, range) -> () // allow?
  | SynExpr.JoinIn(lhsExpr, lhsRange, rhsExpr, range) -> () // allow?
  | SynExpr.DoBang(expr, range) -> () // allow?
  | SynExpr.LetOrUseBang(bindSeqPoint, isUse, isFromSource, pattern, rhsExpr, andBangs, body, range) -> () // allow?
  | SynExpr.MatchBang(matchSeqPoint, expr, clauses, range) -> () // allow?
  | SynExpr.LongIdentSet(longDotId, expr, range) -> () // allow?
  | SynExpr.YieldOrReturn(flags, expr, range) -> ()
  | SynExpr.YieldOrReturnFrom(flags, expr, range) -> ()

  (* INTERNAL AST EXPRESSIONS *)
  | SynExpr.LibraryOnlyILAssembly(ilCode, typeArgs, args, retTy, range) -> ()
  | SynExpr.LibraryOnlyStaticOptimization(constraints, expr, optimizedExpr, range) -> ()
  | SynExpr.LibraryOnlyUnionCaseFieldGet(expr, longId, fieldNum, range) -> ()
  | SynExpr.LibraryOnlyUnionCaseFieldSet(expr, longId, fieldNum, rhsExpr, range) -> ()
  | SynExpr.SequentialOrImplicitYield(seqPoint, expr1, expr2, ifNotStmt, range) -> ()
  | SynExpr.ImplicitZero(range) -> () // parse error
  | SynExpr.ArbitraryAfterError(debugStr, range) -> () // parse error
  | SynExpr.DiscardAfterMissingQualificationAfterDot(expr, range) -> () // parse error
  | SynExpr.FromParseError(expr, range) -> ()

    
  

  

/// Walk over a list of declarations in a module. This is anything
/// that you can write as a top-level inside module (let bindings,
/// nested modules, type declarations etc.)
let visitDeclarations decls =
  
  for declaration in decls do
    match declaration with
    | SynModuleDecl.Let(isRec, bindings, range) ->
        // Let binding as a declaration is similar to let binding
        // as an expression (in checkExpression), but has no body
        if (isRec) then printfn "Decl is rec"
        let mutable i = 0
        for binding in bindings do
          let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc,
                       data, pat, retInfo, body, m, sp)) = binding
          i <- i + 1
          printfn "%s" "\n"
          printfn "%s %d" "Decl Binding:" i
          visitPattern pat          
          printfn "Parent Binding Pat: %A" pat
          printfn "ParentBinding Args: %A" data        
          //printfn "Binding body expr:\n %A" body

          
          checkExpression body
    | _ -> printfn " - declaration: %A" declaration
    | SynModuleDecl.Attributes(attributes, range) -> ()
    | SynModuleDecl.DoExpr(spInfo, expr, range) -> ()
    | SynModuleDecl.Exception(exnDefn, range) -> ()
    | SynModuleDecl.HashDirective(hashDirective, range) -> ()
    | SynModuleDecl.Let(isRecursive, bindings, range) -> ()
    | SynModuleDecl.ModuleAbbrev(ident, longIdent, range) -> ()
    | SynModuleDecl.NamespaceFragment(fragment) -> ()
    | SynModuleDecl.NestedModule(moduleInfo, isRecursive, decls, isContinuing, range) -> ()
    | SynModuleDecl.Open(target, range) -> ()
    | SynModuleDecl.Types(typeDefns, range) -> ()

/// Walk over all module or namespace declarations
/// (basically 'module Foo =' or 'namespace Foo.Bar')
/// Note that there is one implicitly, even if the file
/// does not explicitly define it..
let visitModulesAndNamespaces modulesOrNss =
  for moduleOrNs in modulesOrNss do
    let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, m)) = moduleOrNs
    printfn "Namespace or module: %A" lid
    visitDeclarations decls


let getProjectOptions mainFile allFiles =
    let mainSource = SourceText.ofString (File.ReadAllText(mainFile))
    let projOptions, errors =
        checker.GetProjectOptionsFromScript(mainFile, mainSource, assumeDotNetFramework = false)
        |> Async.RunSynchronously
    
    let upatedProjOptions = { projOptions with SourceFiles = List.toArray allFiles; OtherOptions = Array.append projOptions.OtherOptions [|"--optimize-"|] } 
    upatedProjOptions

[<EntryPoint>]
let main argv =

    

    // Sample input for the compiler service
    let files = ["FRPLibrary.fs"; "TestInput.fs"]

    let mainFile = files.Item(files.Length - 1)
    let projectOptions = getProjectOptions mainFile files
    let parsingOptions, _errors = checker.GetParsingOptionsFromProjectOptions(projectOptions)

    printfn "%s" "Parsing input files..."
    let parseFileResultsAndFilenames = (List.map (fun file -> 
        // Run the first phase (untyped parsing) of the compiler
        let source = SourceText.ofString (File.ReadAllText(file))
        (checker.ParseFile(file, source, parsingOptions)|> Async.RunSynchronously, file)
      ) files)

    let initialParseErrors = 
        (List.collect (fun (parseFileResult: FSharpParseFileResults) ->
            Array.toList parseFileResult.Errors
        ) (List.map fst parseFileResultsAndFilenames))

    printfn "Parse errors: %A" initialParseErrors

    if initialParseErrors.Length > 0 then
        (List.iter (fun (pe: FSharpErrorInfo) ->
                    printfn "%A" pe
        ) initialParseErrors)
        0
    else 
        // Extract implementation file details
        printfn "%s" "Converting AST..."
        let convertedTreesAndFilenamesAndErrors = 
            (List.map (fun (parseFileResult: FSharpParseFileResults, filename: string) ->
                match parseFileResult.ParseTree with
                | Some(ParsedInput.ImplFile(implFile)) ->
                    // Extract declarations and walk over them
                    let (ParsedImplFileInput(fn, script, name, scopedPragmas, hashDirectives, modules, isLastCompiland)) = implFile
                    match ASTConverter.convertModulesAndNamespaces modules with
                    | ASTConverter.ASTConversionResult.Result(newModules, errors) -> // NO CONVERSION CURRENTLY
                    //visitModulesAndNamespaces modules
                    (ParsedInput.ImplFile(ParsedImplFileInput(fn, script, name, scopedPragmas, hashDirectives, modules, isLastCompiland)), filename, errors)

                | _ -> failwith "F# Interface file (*.fsi) not supported."
            ) parseFileResultsAndFilenames)
    
        let convertedTreesAndFilenames = List.map (fun (ast, filename, _) -> (ast, filename)) convertedTreesAndFilenamesAndErrors
        let conversionErrors = List.collect (fun (_, _, errors) -> errors) convertedTreesAndFilenamesAndErrors

        if conversionErrors.Length > 0 then
            (List.iter (fun (e: string) ->
                printfn "%s" e
            ) conversionErrors)
            0
        else

        let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true } // no comments
        //let ctx = CodeFormatterImpl.createFormatContext file (SourceOrigin.SourceString(""))

        printfn "%s" "Converting AST to strings..."

        let convertedInputTextsAndFilenames = 
            List.map (fun (convertedTree: ParsedInput, filename: string) ->
                (SourceText.ofString (CodeFormatter.FormatASTAsync(convertedTree, filename, [], None, cfg) |> Async.RunSynchronously), filename)
            ) convertedTreesAndFilenames

        printfn "STRINGS: %A" convertedInputTextsAndFilenames

        printfn "%s" "Parsing and typechecking strings..."

        // TEMP
        let inputTextsAndFilenamesTODO = (List.map (fun file -> 
            let source = SourceText.ofString (File.ReadAllText(file))
            (source, file)
            ) files)

        let parseAndTypeCheckResults =
            (List.map (fun (source: ISourceText, filename: string) ->
                //let source = SourceText.ofString (File.ReadAllText(filename)) //TEST
                checker.ParseAndCheckFileInProject(filename, 1, source, projectOptions) |> Async.RunSynchronously
            ) inputTextsAndFilenamesTODO)

        let fsharpTypecheckResults = 
            (List.map (fun (a: FSharpCheckFileAnswer) ->
                match a with
                | FSharpCheckFileAnswer.Succeeded(checkFileResults) ->
                    checkFileResults
            ) (List.map snd parseAndTypeCheckResults))

        let (fsharpTypeCheckWarnings, fsharpTypeCheckErrors) =
            (List.fold (fun ((warnings, errors)) (r: FSharpCheckFileResults) ->
                let errorFilter severity (e: FSharpErrorInfo) = severity = e.Severity
                let errorList = Array.toList r.Errors
                let newWarnings = (List.filter (errorFilter FSharpErrorSeverity.Warning) errorList)
                let newErrors = (List.filter (errorFilter FSharpErrorSeverity.Error) errorList)
                (List.append newWarnings warnings, List.append newErrors errors)
            ) ([], []) fsharpTypecheckResults)

        do if fsharpTypeCheckWarnings.Length > 0 then
            printfn "Typecheck warnings:"
            printList fsharpTypeCheckWarnings


        if fsharpTypeCheckErrors.Length > 0 then
            printfn "Typecheck errors:"
            printList fsharpTypeCheckErrors
            0
        else 
            printfn "%s" "Running Simply RaTT checks..."
            let simplyRattCheckErrors = 
                (List.collect (fun (checkFileResults: FSharpCheckFileResults) ->
                    match checkFileResults.ImplementationFile with 
                    | Some(implementationFileContents) -> 
                        printfn "Checking file %s" implementationFileContents.FileName
                        SimplyRaTTChecker.checkImplementationFile implementationFileContents
                    | _ -> ["NO IMPLEMENTATION FILE CONTENTS"]
                ) fsharpTypecheckResults)

            if simplyRattCheckErrors.Length > 0 then 
                printfn "FSRP errors:"
                printList simplyRattCheckErrors
                0
            else
                let asts = 
                    (List.map (fun (parseResult: FSharpParseFileResults) -> 
                        match parseResult.ParseTree with 
                        | Some(ast) -> ast
                    ) (List.map fst parseAndTypeCheckResults))
                printfn "%s" "Compiling ASTs.."
                let (errors, exitCode) = checker.Compile(asts, "out.dll", "out.exe", [], "", false, false, "") |> Async.RunSynchronously
                printfn "Compilation errors: %A" errors
                if errors.Length > 0 then
                    (List.iter (fun (pe: FSharpErrorInfo) ->
                        printfn "%A" pe
                    ) (Array.toList errors))
                    0
                else 0

        //let (errors, exitCode) = checker.Compile([convertedTree], "out.dll", "out.exe", [], "", false, false, "") |> Async.RunSynchronously
        //printfn "%A" errors

        //let typedAST = parseAndCheckSingleFile convertedInput 
        //printfn "Number of errors: %A" typedAST.Errorsl


        //let out = CodeFormatterImpl.formatAST convertedTree [] ctx cfg
    


