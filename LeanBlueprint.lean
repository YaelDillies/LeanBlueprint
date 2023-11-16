-- This module serves as the root of the `LeanBlueprint` library.
-- Import modules here that should be built as part of the library.
import «LeanBlueprint».Basic

open Lean

def main (args : List String) : IO Unit := do
  let [inputFile, lakeFile, outputFile] := args | 
    IO.throwServerError "Supply an input file, lakefile and output file path."
  let result ← analyzeInput inputFile lakeFile
  let output := toJson result 
  IO.FS.writeFile outputFile (toString output)