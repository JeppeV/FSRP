module Program

open System.IO
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Utility

let checker = FSharpChecker.Create(keepAssemblyContents=true)

[<EntryPoint>]
let main argv = 
    if Array.isEmpty argv then
        printfn "Argument list cannot be empty, must provide the path to at least one .fsproj file"
        -1
    else
        printfn "Starting the FSRP checker...\n"
        let toolsPath = Ionide.ProjInfo.Init.init ()
        let workspaceLoader = Ionide.ProjInfo.WorkspaceLoader.Create toolsPath
        let projPaths = (Array.toList (Array.map (fun p -> System.Environment.CurrentDirectory + "\\" + p) argv))

        let pathsErrors = 
            (List.fold (fun errors p -> 
                if File.Exists (p) then errors
                else $"Unable to locate project file: {p}"::errors
            ) [] projPaths) 

        if not pathsErrors.IsEmpty then 
            printList pathsErrors
            -1
        else

            let projectOptions = workspaceLoader.LoadProjects(projPaths)
            let fcsProjectOptions = Seq.toList (Seq.map (fun po -> Ionide.ProjInfo.FCS.mapToFSharpProjectOptions po projectOptions) projectOptions)

            printfn "Parsing and typechecking...\n"

            let fcsProjectOptionsAndSourceAndFilenames = 
                List.collect (fun (projectOptions : FSharpProjectOptions) -> 
                    let sourceFiles = Array.toList projectOptions.SourceFiles
                    List.map (fun filename -> 
                        let source = SourceText.ofString (File.ReadAllText(filename))
                        (projectOptions, filename, source)
                    ) sourceFiles
                ) fcsProjectOptions

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
                    | None -> (res, $"Typechecking of file %s{filename} aborted" :: errors)
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

                if fsharpTypeCheckErrors.Length > 0 then
                    printfn "F# Typechecker errors:"
                    printList fsharpTypeCheckErrors
                    printfn "Please resolve F# typing errors before running the FSRP checker"
                    -1
                else 
                    printfn "Gathering information about recursive declarations...\n"

                    let mutualRecursionNamesMapResults = 
                        (List.map (fun (parseFileResult: FSharpParseFileResults, checkFileResults: FSharpCheckFileResults, filename: string) ->
                            let (mutualRecursionNamesMap, errors) =
                                match parseFileResult.ParseTree with 
                                | Some(parsedInput) -> FSRPRecursionChecker.topLevelMutualRecursionChecker parsedInput
                                | _ -> failwith $"Missing parse tree for file {filename}"
                            ((parseFileResult, checkFileResults, filename, mutualRecursionNamesMap), errors)
                        ) fsharpTypecheckResultsAllSucceeded)

                    let mutualRecursionErrors = List.collect snd mutualRecursionNamesMapResults
                    let mutualRecursionResultsResults = List.map fst mutualRecursionNamesMapResults

                    if mutualRecursionErrors.Length > 0 then
                        printfn "Recursive declarations errors:"
                        printList mutualRecursionErrors
                        -1
                    else

                        printfn "%s" "Running FSRP checks...\n"
                        let simplyRattCheckResults = 
                            (List.collect (fun  (_, checkFileResults : FSharpCheckFileResults, _, mutualRecursionNamesMap)  ->
                                match checkFileResults.ImplementationFile with 
                                | Some(implementationFileContents) -> 
                                    printfn "Checking file %s" implementationFileContents.FileName
                                    (FSRPTypeChecker.checkImplementationFile implementationFileContents mutualRecursionNamesMap)
                                | _ -> failwith "NO IMPLEMENTATION FILE CONTENTS"
                            ) mutualRecursionResultsResults)


                        let (simplyRattCheckWarnings, simplyRattCheckErrors) =
                            (List.fold (fun ((warnings, errors)) (simplyRattCheckResult : FSRPError.FSRPError)  ->
                                match simplyRattCheckResult.severity with
                                | FSRPError.FSRPErrorSeverity.Warning -> (simplyRattCheckResult :: warnings, errors)
                                | FSRPError.FSRPErrorSeverity.Error -> (warnings, simplyRattCheckResult :: errors)
                            ) ([], []) simplyRattCheckResults)

                        do if not simplyRattCheckWarnings.IsEmpty then
                            printfn "\nFSRP warnings:"
                            printList simplyRattCheckWarnings

                        if not simplyRattCheckErrors.IsEmpty then 
                            printfn "\nFSRP errors:"
                            printList simplyRattCheckErrors
                            -1
                        else 0
    


