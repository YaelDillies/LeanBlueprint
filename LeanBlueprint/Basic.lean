import LeanBlueprint.LeanContext

open Lean Elab System

deriving instance ToJson, FromJson for Position

structure DeclDetails where
  declName : Name
  pos : Position
  hasSorry : Bool
deriving ToJson, FromJson, Repr


def extractId (stx : Syntax) : Option Name := do
  let declId ← stx.find? (·.getKind == `Lean.Parser.Command.declId)
  let id ← declId.getHead?
  return id.getId

partial def resolveDeclsAux (fileMap : FileMap) (acc : Array DeclDetails := #[]) : InfoTree → IO (Array DeclDetails)
  | .context _ctx tree => resolveDeclsAux fileMap acc tree
  | tree@(.node info children) => do
    let childLeaves ← children.toArray.foldlM (init := acc) (resolveDeclsAux fileMap ·)
    let hasSorry ← tree.hasSorry
    let declDetails? : Option DeclDetails := do
      let .ofCommandInfo cmdInfo := info | none
      guard <| cmdInfo.elaborator == `Lean.Elab.Command.elabDeclaration
      let decl := cmdInfo.stx
      let id ← extractId decl
      let pos ← decl.getPos? true
      return ⟨id, fileMap.toPosition pos, hasSorry⟩
    match declDetails? with
    | some declDetails => return childLeaves.push declDetails
    | none => return childLeaves
  | _ => return acc

def resolveDecls (fileMap : FileMap) (trees : Array InfoTree) : IO (Array DeclDetails) := do
  trees.foldlM (init := #[]) (resolveDeclsAux fileMap ·) 

def analyzeInput (file lakeFile : System.FilePath) : IO <| Array DeclDetails := do
  let fileContents ← IO.FS.readFile file
  let fileMap := FileMap.ofString fileContents 
  -- Parse the header of the provided file
  let context := Parser.mkInputContext fileContents file.toString
  let (header, state, messages) ← Parser.parseHeader context
  -- Load the imports
  initializeLakeContext lakeFile header
  let (environment, messages) ← processHeader header {} messages context 0
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        IO.throwServerError =<< msg.toString
  -- Process the remaining file
  let commandState := { Command.mkState environment messages with infoState := { enabled := true } }
  let s ← IO.processCommands context state commandState
  -- Resolve the list of declarations from the file's infotrees
  resolveDecls fileMap s.commandState.infoState.trees.toArray

#eval analyzeInput "./LeanBlueprint/Test.lean" "./lakefile.lean"