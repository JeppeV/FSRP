module Utility

open FSharp.Compiler.Range

let rec appendMany (ls: 'a list list) =
    match ls with
    | hd::tl -> List.append hd (appendMany tl)
    | [] ->  []

let doOrEmptyList (opt: Option<'a>) (f: 'a -> 'b list) : 'b list =
    match opt with
    | Some(a) -> f(a)
    | None -> []

let formatErrorPrefix (r: range) : string =
    $"%s{r.FileName} %A{r.Start}-%A{r.End} FSRP error"


let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])


