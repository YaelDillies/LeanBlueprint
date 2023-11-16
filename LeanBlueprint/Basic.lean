import Lean

open Lean Elab System

instance : ToJson String.Pos where
  toJson pos := toJson pos.byteIdx 

instance : FromJson String.Pos where
  fromJson? jsn := do
    let n : Nat ← fromJson? jsn
    return ⟨n⟩ 

structure DeclDetails where
  declName : Name
  pos : String.Pos
  hasSorry : Bool
deriving ToJson, FromJson, Repr


def extractId (stx : Syntax) : Option Name := do
  let declId ← stx.find? (·.getKind == `Lean.Parser.Command.declId)
  let id ← declId.getHead?
  return id.getId

partial def resolveDeclsAux (aux : Array DeclDetails := #[]) : InfoTree → IO (Array DeclDetails)
  | .context _ctx tree => resolveDeclsAux aux tree
  | tree@(.node info children) => do
    let childLeaves ← children.toArray.foldlM (init := aux) (resolveDeclsAux ·)
    let hasSorry ← tree.hasSorry 
    match info with
      | .ofCommandInfo cmdInfo => 
        if cmdInfo.elaborator == `Lean.Elab.Command.elabDeclaration then
          let decl := cmdInfo.stx
          let declDetails : Option DeclDetails := do
            let id ← extractId decl
            let pos ← decl.getPos? true
            return ⟨id, pos, hasSorry⟩
          match declDetails with
          | some declDetails => return childLeaves.push declDetails
          | none => return childLeaves
        else
          return childLeaves
      | _ => return childLeaves
  | _ => return aux

def resolveDecls (trees : Array InfoTree) : IO (Array DeclDetails) := do
  trees.foldlM (init := #[]) (resolveDeclsAux ·) 

def analyzeInput (file : System.FilePath) : IO <| Array DeclDetails := do
  let fileContents ← IO.FS.readFile file
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
  let result ← resolveDecls s.commandState.infoState.trees.toArray
  return result

#eval show IO _ from do
  let result ← analyzeInput "./LeanBlueprint/Test.lean"
  return result