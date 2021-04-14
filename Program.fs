module Program

open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text

// Create an interactive checker instance
let checker = FSharpChecker.Create(keepAssemblyContents=true)

let printList (l: 'T list) =
    List.iter (fun o -> printfn "%A" o) l
  
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
    let files = ["FRPLibrary.fs"]

    let mainFile = files.Item(files.Length - 1)
    let projectOptions = getProjectOptions mainFile files

    printfn "%s" "Parsing and typechecking strings..."

    let inputTextsAndFilenames = (List.map (fun filename -> 
        let source = SourceText.ofString (File.ReadAllText(filename))
        (source, filename)
        ) files)

    let parseAndTypeCheckResultsAndFilenames =
        (List.map (fun (source: ISourceText, filename: string) ->
            //let source = SourceText.ofString (File.ReadAllText(filename)) //TEST
            (checker.ParseAndCheckFileInProject(filename, 1, source, projectOptions) |> Async.RunSynchronously, filename)
        ) inputTextsAndFilenames)

    let fsharpTypecheckResults = 
        (List.map (fun ((parseFileResults, checkFileAnswer), filename) ->
            match checkFileAnswer with
            | FSharpCheckFileAnswer.Succeeded(checkFileResults) ->
                (parseFileResults, Some(checkFileResults), filename)
            | FSharpCheckFileAnswer.Aborted ->
                (parseFileResults, None, filename)
                
        ) parseAndTypeCheckResultsAndFilenames)

    let (fsharpTypecheckResultsAllSucceeded, typeCheckProcessErrors) = 
        List.fold (fun (res, errors) (parseFileResults, checkFileResultsOpt, filename) ->
            match checkFileResultsOpt with
            | None -> (res, $"Typecheck of file %s{filename} aborted" :: errors)
            | Some(checkFileResults) -> ((parseFileResults, checkFileResults, filename)::res, errors)
        ) ([],[]) fsharpTypecheckResults

    if typeCheckProcessErrors.Length > 0 then
        printList typeCheckProcessErrors
        -1
    else

        let (fsharpTypeCheckWarnings, fsharpTypeCheckErrors) =
            (List.fold (fun ((warnings, errors)) ((_, checkFileResults, _) : FSharpParseFileResults * FSharpCheckFileResults * string)  ->
                let errorFilter severity (e: FSharpErrorInfo) = severity = e.Severity
                let errorList = Array.toList checkFileResults.Errors
                let newWarnings = (List.filter (errorFilter FSharpErrorSeverity.Warning) errorList)
                let newErrors = (List.filter (errorFilter FSharpErrorSeverity.Error) errorList)
                (List.append newWarnings warnings, List.append newErrors errors)
            ) ([], []) fsharpTypecheckResultsAllSucceeded)

        do if fsharpTypeCheckWarnings.Length > 0 then
            printfn "Typecheck warnings:"
            printList fsharpTypeCheckWarnings


        if fsharpTypeCheckErrors.Length > 0 then
            printfn "Typecheck errors:"
            printList fsharpTypeCheckErrors
            -1
        else 

            let mutualRecursionNamesMapResults = 
                (List.map (fun (parseFileResult: FSharpParseFileResults, checkFileResults: FSharpCheckFileResults, filename: string) ->
                    let (mutualRecursionNamesMap, errors) = PurityChecker.topLevelMutualRecursionChecker parseFileResult
                    ((parseFileResult, checkFileResults, filename, mutualRecursionNamesMap), errors)
                ) fsharpTypecheckResultsAllSucceeded)

            let mutualRecursionErrors = List.collect snd mutualRecursionNamesMapResults
            let mutualRecursionResultsResults = List.map fst mutualRecursionNamesMapResults

            if mutualRecursionErrors.Length > 0 then
                printfn "Recursive declaration errors:"
                printList mutualRecursionErrors
                -1
            else

                printfn "PurityAndmutualRecursionNamesMapResults: %A" mutualRecursionResultsResults

                printfn "%s" "Running Simply RaTT checks..."
                let simplyRattCheckErrors = 
                    (List.collect (fun  (_, checkFileResults : FSharpCheckFileResults, _, mutualRecursionNamesMap)  ->
                        match checkFileResults.ImplementationFile with 
                        | Some(implementationFileContents) -> 
                            printfn "Checking file %s" implementationFileContents.FileName
                            (SimplyRaTTChecker.checkImplementationFile implementationFileContents mutualRecursionNamesMap)
                        | _ -> failwith "NO IMPLEMENTATION FILE CONTENTS"
                    ) mutualRecursionResultsResults)

                if simplyRattCheckErrors.Length > 0 then 
                    printfn "FSRP errors:"
                    printList simplyRattCheckErrors
                    -1
                else
                    let asts = 
                        (List.map (fun (parseFileResults : FSharpParseFileResults, checkFileResults : FSharpCheckFileResults, filename, mutualRecursionNamesMap)  -> 
                            match parseFileResults.ParseTree with 
                            | Some(ast) -> ast
                        ) mutualRecursionResultsResults)
                    printfn "%s" "Compiling ASTs.."
                    let (errors, exitCode) = checker.Compile(asts, "out.dll", "out.exe", [], "", false, false, "") |> Async.RunSynchronously
                    printfn "%A" "Compilation errors:"
                    if errors.Length > 0 then
                        (List.iter (fun (pe: FSharpErrorInfo) ->
                            printfn "%A" pe
                        ) (Array.toList errors))
                        exitCode
                    else 0
    


