module FSRPError

open FSharp.Compiler.Text

type FSRPErrorSeverity = 
    | Warning
    | Error

type FSRPError (severity: FSRPErrorSeverity, message: string, range: range) =
    member val severity = severity
    member val message = message
    member val range = range
    override self.ToString() =
        let severityString = 
            match severity with 
            | FSRPErrorSeverity.Warning -> "warning" 
            | FSRPErrorSeverity.Error -> "error"
        $"%s{range.FileName} %A{range.Start}-%A{range.End} FSRP %s{severityString}: %s{message}"
        
let makeWarning message range =
    new FSRPError(FSRPErrorSeverity.Warning, message, range)

let makeError message range = 
    new FSRPError(FSRPErrorSeverity.Error, message, range)

