module FSRPAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Compiler.SourceCodeServices
open FSharp.Core
open FSharp.Compiler.Text
open System
open FSRPError

[<Analyzer>]
let fsrpAnalyzer : Analyzer =
    fun ctx ->
        let (mutRecInfo, mutRecErrors) = MutualRecusionChecker.topLevelMutualRecursionChecker ctx.ParseTree
        let fsrpErrors = SimplyRaTTChecker.checkImplementationFile ctx.TypedTree mutRecInfo
        (List.append mutRecErrors fsrpErrors)
        |> Seq.map (fun e ->
            { Type = "FSRP Analyzer"
              Message = e.message
              Code = "FSRP6"
              Severity = match e.severity with | FSRPErrorSeverity.Warning-> Severity.Warning | FSRPErrorSeverity.Error -> Severity.Error 
              Range = e.range
              Fixes = []}
        )
        |> Seq.toList