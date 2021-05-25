module Utility

let rec appendMany (ls: 'a list list) =
    match ls with
    | hd::tl -> List.append hd (appendMany tl)
    | [] ->  []

let rec lastInList (l: 'a list) : 'a =
    match l with
    | [hd] -> hd
    | _::tl -> lastInList tl
    | [] -> failwith "Cannot retrieve last element of empty list"

let printList (l: 'T list) =
    List.iter (fun o -> printfn "%A" o) (List.rev l)

let doOrEmptyList (opt: Option<'a>) (f: 'a -> 'b list) : 'b list =
    match opt with
    | Some(a) -> f(a)
    | None -> []

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])


