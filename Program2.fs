module Program2

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text

type FunInfo = {
    FunName: string
    IsRecApp: bool
    InitialArgs: Set<string>
}

type Environment = {
    FunInfo: FunInfo
    InitialEnv: Set<string>
    Now: bool
    NowEnv: Set<string>
    Later: bool
    LaterEnv: Set<string>
} 

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let projectOptions projFileName inputFiles outputDllName =
    let sysLib nm =
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\" + nm + ".dll"
        else
            let sysDir = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
            let (++) a b = System.IO.Path.Combine(a,b)
            sysDir ++ nm + ".dll"

    let fsCore4300() =
        if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
            // file references only valid on Windows
            //System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86) +
            //@"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.0.0\FSharp.Core.dll"
            "FSharp.Core.dll"
        else
            sysLib "FSharp.Core"


    checker.GetProjectOptionsFromCommandLineArgs
       (projFileName,
        [| yield "--simpleresolution"
           yield "--noframework"
           yield "--debug:full"
           yield "--define:DEBUG"
           yield "--optimize-"
           yield "--out:" + outputDllName
           yield "--doc:test.xml"
           yield "--warn:3"
           yield "--fullpaths"
           yield "--flaterrors"
           yield "--target:library"
           for f in inputFiles do
                 yield f
           let references =
             [ sysLib "mscorlib"
               sysLib "System"
               sysLib "System.Core"
               fsCore4300() ]
           for r in references do
                 yield "-r:" + r |])

let parseAndCheckProject projectOptions = 
    checker.ParseAndCheckProject (projectOptions)
    |> Async.RunSynchronously


let rec printDecl prefix d =
    match d with
    | FSharpImplementationFileDeclaration.Entity (e, subDecls) ->
        printfn "%sEntity %s was declared and contains %d sub-declarations" prefix e.CompiledName subDecls.Length
        for subDecl in subDecls do
            printDecl (prefix+"    ") subDecl
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) ->
        printfn "%sMember or value %s was declared" prefix  v.CompiledName
    | FSharpImplementationFileDeclaration.InitAction(e) ->
        printfn "%sA top-level expression was declared" prefix





//[<EntryPoint>]
let main argv =
    let file = "/home/user/Test.fsx"
    let filename = "TestInput.fs"
    let input = System.IO.File.ReadAllText(filename)

    let filenames = ["FRPLibrary.fs"; "TestInput.fs"]

    let lastfile = filenames.[filenames.Length-1]
    let dllName = System.IO.Path.ChangeExtension(lastfile, ".dll")
    let projFileName = System.IO.Path.ChangeExtension(lastfile, ".fsproj")


    let (options, errors) = checker.GetProjectOptionsFromScript(filename, SourceText.ofString input, assumeDotNetFramework = false) |> Async.RunSynchronously
    let upatedOptions = { options with SourceFiles = List.toArray filenames; OtherOptions = Array.append options.OtherOptions (List.toArray ["--optimize-"]); IsIncompleteTypeCheckEnvironment = true } 
    printfn "%A" options.SourceFiles
    printfn "%A" "________"
    
    let checkProjectResults = parseAndCheckProject(upatedOptions)

    printfn "%A" checkProjectResults.Errors

    let backgroundParseResults1, backgroundTypedParse1 =
        checker.GetBackgroundCheckResultsForFileInProject("TestInput.fs", options)
        |> Async.RunSynchronously
    //let assemblyContent = checkProjectResults.
    
    let checkedFileOpt = backgroundTypedParse1.ImplementationFile

    printfn "Errors: %A" checkProjectResults.Errors
    
    match checkedFileOpt with
    | Some(checkedFile) -> 

    for d in checkedFile.Declarations do
        printDecl "" d

    let myLibraryEntity, myLibraryDecls =
        match checkedFile.Declarations.[0] with
        | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> (e, subDecls)
        | _ -> failwith "unexpected"


    //printfn "Top level decls %A" checkedFile.Declarations

    //printfn "Declarations %A" myLibraryDecls

    let (fooSymbol, fooArgs, fooExpression) =
        match myLibraryDecls.[0] with
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(v, vs, e) -> 
            do printfn "Attributes %A" v.Attributes
            do printfn "Args %A" vs
            (v, vs, e)
        //| _ -> failwith "unexpected"
    
    
    let initialArgs = fooArgs.[0]
    let nowArgs = fooArgs.[1]
    
    let folder (acc: Set<string>) (a : FSharpMemberOrFunctionOrValue) = 
        if a.IsCompilerGenerated then  
            acc
        else
            acc.Add(a.FullName)

    let initialEnv = List.fold folder (Set([])) initialArgs
    let nowEnv = List.fold folder (Set([fooSymbol.LogicalName])) nowArgs

    let env : Environment = {
        FunInfo = {
            FunName = fooSymbol.LogicalName
            InitialArgs = initialEnv;
            IsRecApp = false;
        }    
        InitialEnv = initialEnv;
        Now = true;
        NowEnv = nowEnv
        Later = false;
        LaterEnv = Set([]);
    }

    printfn "Environment: %A" env


    //fooExpression |> checkExpr (fun e -> printfn "Visiting %A" e) env

    
    0

