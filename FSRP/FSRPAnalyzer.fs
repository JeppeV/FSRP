module FSRPAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.SourceCodeServices
open FSharp.Core
open FSharp.Compiler.Text
open System
open FSRPError
open FSharp.Compiler

[<Analyzer "FSRPAnalyzer">]
let fsrpAnalyzer : Analyzer =
    fun ctx ->
        let testError = 
            { 
                Type = "FSRP Analyzer"
                Message = "Analyzer is running"
                Code = "FSRP001"
                Severity = Severity.Warning
                Range = Range.Zero
                Fixes = []
            }
        let (mutRecInfo, mutRecErrors) = MutualRecusionChecker.topLevelMutualRecursionChecker ctx.ParseTree
        let fsrpErrors = SimplyRaTTChecker.checkImplementationFile ctx.TypedTree mutRecInfo
        (testError:: ((List.append mutRecErrors fsrpErrors)
        |> List.map (fun e ->
            { Type = "FSRP Analyzer"
              Message = e.message
              Code = "FSRP001"
              Severity = match e.severity with | FSRPErrorSeverity.Warning-> Severity.Warning | FSRPErrorSeverity.Error -> Severity.Error 
              Range = e.range
              Fixes = []}
        )))