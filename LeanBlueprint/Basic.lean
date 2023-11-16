import Lean

open Lean Elab System

deriving instance ToJson, FromJson for Lean.Position

structure DeclDetails where
  declName : Name
  pos : Lean.Position
  hasSorry : Bool
deriving ToJson, FromJson, Repr


def extractId (stx : Syntax) : Option Name := do
  let declId ← stx.find? (·.getKind == `Lean.Parser.Command.declId)
  let id ← declId.getHead?
  return id.getId

partial def resolveDeclsAux (fileMap : FileMap) (aux : Array DeclDetails := #[]) : InfoTree → IO (Array DeclDetails)
  | .context _ctx tree => resolveDeclsAux fileMap aux tree
  | tree@(.node info children) => do
    let childLeaves ← children.toArray.foldlM (init := aux) (resolveDeclsAux fileMap ·)
    let hasSorry ← tree.hasSorry 
    match info with
      | .ofCommandInfo cmdInfo => 
        if cmdInfo.elaborator == `Lean.Elab.Command.elabDeclaration then
          let decl := cmdInfo.stx
          let declDetails : Option DeclDetails := do
            let id ← extractId decl
            let pos ← decl.getPos? true
            return ⟨id, fileMap.toPosition pos, hasSorry⟩
          match declDetails with
          | some declDetails => return childLeaves.push declDetails
          | none => return childLeaves
        else
          return childLeaves
      | _ => return childLeaves
  | _ => return aux

def resolveDecls (fileMap : FileMap) (trees : Array InfoTree) : IO (Array DeclDetails) := do
  trees.foldlM (init := #[]) (resolveDeclsAux fileMap ·) 

def analyzeInput (file : System.FilePath) : IO <| Array DeclDetails := do
  let fileContents ← IO.FS.readFile file
  let fileMap := FileMap.ofString fileContents 
  -- Parse the header of the provided file
  let context := Parser.mkInputContext fileContents file.toString
  let (header, state, messages) ← Parser.parseHeader context
  let context' := Parser.mkInputContext fileContents file.toString
  -- Load the imports
  -- initializeLakeContext lakeFile header
  let options := Options.empty |>.setBool `trace.Elab.info true |>.setBool `tactic.simp.trace true
  let (environment, messages) ← processHeader header options messages context' 0
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        IO.throwServerError <| ← msg.toString
  -- Process the remaining file
  let commandState := { Command.mkState environment messages with infoState := { enabled := true } }
  let s ← IO.processCommands context' state commandState
  -- Resolve the list of declarations from the file's infotrees
  let result ← resolveDecls fileMap s.commandState.infoState.trees.toArray
  return result

#eval analyzeInput "./LeanBlueprint/Test.lean"