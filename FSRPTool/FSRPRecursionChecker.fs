module FSRPRecursionChecker

open FSharp.Compiler.SyntaxTree
open Utility
open FSRPError

    module private Internal =
            
        let longIdentToString (lident: LongIdent) : string =
            String.concat "." (List.map (fun (ident: Ident) -> ident.idText) lident)
            
        let isFSRPFunction (attrs: SynAttributes) =
            (List.exists (fun (attrsList: SynAttributeList) -> 
                (List.exists (fun (attr: SynAttribute) -> 
                    match attr.TypeName with   
                    | LongIdentWithDots(idents, _) -> 
                        let ident = lastInList idents
                        ident.idText = "FSRP"
                ) attrsList.Attributes)
            ) attrs)
          
        let getFunNameFromPattern (pat: SynPat) = 
            match pat with 
            | SynPat.Named(pat, name, _, _, _) ->
                name.idText
            | SynPat.LongIdent(LongIdentWithDots(idents, _), _ , _, _, _, _) ->
               (String.concat "." (Seq.map (fun (i: Ident) -> i.idText) idents))
                
            | _ -> failwith $"Recursive function binding has pattern %A{pat}.   (handle it)"
        
        let collectAllBindingNamesAndFSRPCount (decls: SynModuleDecl list) =
            
            List.fold (fun (declsAcc, declsFsrpCount) decl -> 
                match decl with
                | SynModuleDecl.Let(isRec, bindings, range) ->
                    let (bindingsNames, fsrpCount) = 
                        List.fold (fun (bindingsAcc, fsrpCount) binding ->
                            let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
                            ((getFunNameFromPattern pat)::bindingsAcc, if isFSRPFunction attrs then fsrpCount + 1 else fsrpCount)
                        ) ([], 0) bindings
                    (List.append bindingsNames declsAcc, fsrpCount + declsFsrpCount)
                | _ -> (declsAcc, declsFsrpCount)
            ) ([], 0) decls
        
        
        
        let rec checkModuleOrNamespaceDeclarations decls currentModuleName isRecModule moduleRange =
        
        
            let (allDeclsRecFunNames, fullFsrpCount) = 
                if isRecModule then 
                    collectAllBindingNamesAndFSRPCount decls
                else ([], -1) // this is never used
        
            let declarationFolder ((mutualRecursionNamesMap, errors) : Map<string, string list> * FSRPError list) (declaration:  SynModuleDecl) =
                match declaration with
                | SynModuleDecl.Let(isRec, bindings, range) ->
                    let bindingFolder ((recFunNames, fsrpCount) : string list * int) (binding:  SynBinding) =
                        let (Binding(access, kind, inlin, mutabl, attrs, xmlDoc, data, pat, retInfo, body, m, sp)) = binding
                        let isFSRPFunction = isFSRPFunction attrs
                        let recFunNameOpt = 
                            if isRec || isRecModule then
                                Some(getFunNameFromPattern pat)
                            else None
        
                        let newFsrpCount =  if isFSRPFunction then fsrpCount + 1 else fsrpCount
                        
                        let newRecFunNames =
                            match recFunNameOpt with
                            | Some(recFunName) -> (currentModuleName + "." + recFunName :: recFunNames)
                            | None -> recFunNames
        
                        (newRecFunNames, newFsrpCount)
                    
                    let (recFunNames, fsrpCount) = (List.fold bindingFolder ([], 0) bindings)
        
                    let mutRecErrors = 
                        if (not isRecModule) && (fsrpCount > 0 && fsrpCount <> bindings.Length) then
                            [makeError "Either all or no let bindings in mutual recusion block must have FSRP attribute" range]
                        else []
        
                    let moreMutualRecursionNamesMap = 
                        let res = 
                            if isRecModule then                        
                                List.fold (fun (acc : Map<string,string list>) (recFunName : string) -> acc.Add(recFunName, allDeclsRecFunNames)) (Map([])) recFunNames
                            else if isRec then 
                                List.fold (fun (acc : Map<string,string list>) (recFunName : string) -> acc.Add(recFunName, recFunNames)) (Map([])) recFunNames
                            else 
                                Map([])
                        res
        
                    (joinMaps mutualRecursionNamesMap moreMutualRecursionNamesMap, List.append mutRecErrors errors)
                
                | SynModuleDecl.NestedModule(ComponentInfo(attrs, _, _, lident, _, _, _, _), isRecursive, decls, isContinuing, range) -> 
                    let (moreMutualRecursionNamesMap, errors') = checkModuleOrNamespaceDeclarations decls $"%s{currentModuleName}.%s{(longIdentToString (lident))}" (isRecursive || isRecModule) range
                    (joinMaps mutualRecursionNamesMap moreMutualRecursionNamesMap, List.append errors' errors)
                | _ -> (mutualRecursionNamesMap, errors)
                
                // potentially need to handle these cases
                //| SynModuleDecl.Attributes(attributes, range) -> ()
                //| SynModuleDecl.DoExpr(spInfo, expr, range) -> ()
                //| SynModuleDecl.Exception(exnDefn, range) -> ()
                //| SynModuleDecl.HashDirective(hashDirective, range) -> ()
                //| SynModuleDecl.ModuleAbbrev(ident, longIdent, range) -> ()
                //| SynModuleDecl.NamespaceFragment(fragment) -> ()
                //| SynModuleDecl.Open(target, range) -> ()
                //| SynModuleDecl.Types(typeDefns, range) -> ()
                
            let recModuleErrors = 
                if isRecModule && (fullFsrpCount > 0) && (fullFsrpCount <> allDeclsRecFunNames.Length) then
                    [makeError "Either all or no let bindings in recursive module must have FSRP attribute" moduleRange]
                else []
        
            let (mutRecInfo, errors) = List.fold declarationFolder (Map([]), []) decls
            (mutRecInfo, appendMany [errors; recModuleErrors])
        
        
        let checkModulesOrNamespaces (modulesOrNss : SynModuleOrNamespace list) =
            let folder ((mutualRecursionNamesMaps, errors) : Map<string, string list> * FSRPError list) (modOrNs:  SynModuleOrNamespace) =
                let (SynModuleOrNamespace(lid, isRec, isMod, decls, xml, attrs, _, moduleRange)) = modOrNs
                let (mutualRecursionNamesMaps', errors') = checkModuleOrNamespaceDeclarations decls (longIdentToString lid) isRec moduleRange
                (joinMaps mutualRecursionNamesMaps' mutualRecursionNamesMaps, List.append errors' errors)
        
            let res = List.fold folder (Map([]), []) modulesOrNss
            res
        
let topLevelMutualRecursionChecker (parsedInput: ParsedInput) =
    match parsedInput with
    | ParsedInput.ImplFile(parsedImplFileInput) ->
        let (ParsedImplFileInput(fn, script, name, scopedPragmas, hashDirectives, modules, isLastCompiland)) = parsedImplFileInput
        Internal.checkModulesOrNamespaces modules
    | ParsedInput.SigFile(parsedSigFileInput) -> (Map([]), []) // signature files do not contain expressions (i think)

  