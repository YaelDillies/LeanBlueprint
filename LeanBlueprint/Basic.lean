import LeanBlueprint.LeanContext

open Lean Elab System

deriving instance ToJson, FromJson for Position

structure LocatedDeclDetails where
  declName : Name
  pos : Position
  hasSorry : Bool
deriving ToJson, FromJson, Repr


def extractId (stx : Syntax) : Option Name := do
  let declId ← stx.find? (·.getKind == `Lean.Parser.Command.declId)
  let id ← declId.getHead?
  return id.getId

partial def resolveDeclsAux (fileMap : FileMap) (acc : Array LocatedDeclDetails := #[]) : InfoTree → IO (Array LocatedDeclDetails)
  | .context _ctx tree => resolveDeclsAux fileMap acc tree
  | tree@(.node info children) => do
    let childLeaves ← children.toArray.foldlM (init := acc) (resolveDeclsAux fileMap ·)
    let hasSorry ← tree.hasSorry
    let locatedDeclDetails? : Option LocatedDeclDetails := do
      let .ofCommandInfo cmdInfo := info | none
      guard <| cmdInfo.elaborator == `Lean.Elab.Command.elabDeclaration
      let decl := cmdInfo.stx
      let id ← extractId decl
      let pos ← decl.getPos? true
      return ⟨id, fileMap.toPosition pos, hasSorry⟩
    match locatedDeclDetails? with
    | some locatedDeclDetails => return childLeaves.push locatedDeclDetails
    | none => return childLeaves
  | _ => return acc

def resolveDecls (fileMap : FileMap) (trees : Array InfoTree) : IO (Array LocatedDeclDetails) := do
  trees.foldlM (init := #[]) (resolveDeclsAux fileMap ·) 

def analyzeInput (file lakeFile : System.FilePath) : IO <| Array LocatedDeclDetails := do
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

#eval analyzeInput "./Test/Test.lean" "./lakefile.lean"

def getModuleOf (decl : Name) : MetaM Name := do
  let env ← getEnv
  let .some idx := env.getModuleIdxFor? decl | throwError s!"Could not find module index for {decl}"
  return env.header.moduleNames.get! idx

#eval getModuleOf `Nat
