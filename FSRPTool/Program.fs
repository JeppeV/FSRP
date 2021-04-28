module Program

open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open System.Xml

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let printList (l: 'T list) =
    List.iter (fun o -> printfn "%A" o) (List.rev l)

// commented 20/4 
//let getProjectOptions mainFile allFiles =
//    let mainSource = SourceText.ofString (File.ReadAllText(mainFile))
//    let projOptions, errors =
//        checker.GetProjectOptionsFromScript(mainFile, mainSource, assumeDotNetFramework = false)
//        |> Async.RunSynchronously
    
//    let upatedProjOptions = { projOptions with SourceFiles = List.toArray allFiles(*; OtherOptions = Array.append projOptions.OtherOptions [|"--optimize-"|] *)} 
//    upatedProjOptions

[<EntryPoint>]
let main argv = 
    printfn "Running FSRP checks"
    let toolsPath = Ionide.ProjInfo.Init.init ()
    let workspaceLoader = Ionide.ProjInfo.WorkspaceLoader.Create toolsPath
    let projPaths = (Array.toList (Array.map (fun p -> System.Environment.CurrentDirectory + "\\" + p) argv))

    let pathsErrors = 
        (List.fold (fun errors p -> 
            if File.Exists (p) then errors
            else $"Unable to read project file: {p}"::errors
        ) [] projPaths) 

    if not pathsErrors.IsEmpty then 
        printList pathsErrors
        -1
    else

        let projectOptions = workspaceLoader.LoadProjects(projPaths)
        let fcsProjectOptions = Seq.toList (Seq.map (fun po -> Ionide.ProjInfo.FCS.mapToFSharpProjectOptions po projectOptions) projectOptions)

        printfn "Parsing and typechecking..."

        let fcsProjectOptionsAndSourceAndFilenames = 
            List.collect (fun (projectOptions : FSharpProjectOptions) -> 
                let sourceFiles = Array.toList projectOptions.SourceFiles
                List.map (fun filename -> 
                    let source = SourceText.ofString (File.ReadAllText(filename))
                    (projectOptions, filename, source)
                ) sourceFiles
            ) fcsProjectOptions

        printfn "ProjectOptions: %A" projectOptions

        let parseAndTypeCheckResultsAndFilenames =
            (List.map (fun (projectOptions: FSharpProjectOptions, filename: string, source: ISourceText) ->
                (checker.ParseAndCheckFileInProject(filename, 1, source, projectOptions) |> Async.RunSynchronously, filename)
            ) fcsProjectOptionsAndSourceAndFilenames)

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
                    let errorFilter severity (e: FSharpDiagnostic) = severity = e.Severity
                    let errorList = Array.toList checkFileResults.Errors
                    let newWarnings = (List.filter (errorFilter FSharpDiagnosticSeverity.Warning) errorList)
                    let newErrors = (List.filter (errorFilter FSharpDiagnosticSeverity.Error) errorList)
                    (List.append newWarnings warnings, List.append newErrors errors)
                ) ([], []) fsharpTypecheckResultsAllSucceeded)

            // 20/4
            //do if fsharpTypeCheckWarnings.Length > 0 then
            //    printfn "Typecheck warnings:"
            //    printList fsharpTypeCheckWarnings

            if fsharpTypeCheckErrors.Length > 0 then
                printfn "Typecheck errors:"
                printList fsharpTypeCheckErrors
                printfn "Please resolve typecheck errors before running the FSRP checker"
                -1
            else 

                let mutualRecursionNamesMapResults = 
                    (List.map (fun (parseFileResult: FSharpParseFileResults, checkFileResults: FSharpCheckFileResults, filename: string) ->
                        let (mutualRecursionNamesMap, errors) =
                            match parseFileResult.ParseTree with 
                            | Some(parsedInput) -> MutualRecusionChecker.topLevelMutualRecursionChecker parsedInput
                            | _ -> failwith $"Missing parse tree for file {filename}"
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
                    let simplyRattCheckResults = 
                        (List.collect (fun  (_, checkFileResults : FSharpCheckFileResults, _, mutualRecursionNamesMap)  ->
                            match checkFileResults.ImplementationFile with 
                            | Some(implementationFileContents) -> 
                                printfn "Checking file %s" implementationFileContents.FileName
                                (SimplyRaTTChecker.checkImplementationFile implementationFileContents mutualRecursionNamesMap)
                            | _ -> failwith "NO IMPLEMENTATION FILE CONTENTS"
                        ) mutualRecursionResultsResults)


                    let (simplyRattCheckWarnings, simplyRattCheckErrors) =
                        (List.fold (fun ((warnings, errors)) (simplyRattCheckResult : FSRPError.FSRPError)  ->
                            match simplyRattCheckResult.severity with
                            | FSRPError.FSRPErrorSeverity.Warning -> (simplyRattCheckResult :: warnings, errors)
                            | FSRPError.FSRPErrorSeverity.Error -> (warnings, simplyRattCheckResult :: errors)
                        ) ([], []) simplyRattCheckResults)

                    do if not simplyRattCheckWarnings.IsEmpty then
                        printfn "FSRP warnings:"
                        printList simplyRattCheckWarnings

                    if not simplyRattCheckErrors.IsEmpty then 
                        printfn "FSRP errors:"
                        printList simplyRattCheckErrors
                        -1
                    else 0
    


