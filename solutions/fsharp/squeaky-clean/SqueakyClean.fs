module SqueakyClean

open System
open System.Text.RegularExpressions

let transform (c: char) : string = 
    match c with
    | '-' -> "_"                                      // Replace any hyphens encountered with underscores
    | c when Char.IsWhiteSpace c -> ""                // Remove all whitespace
    | c when Char.IsUpper c -> $"-{Char.ToLower c}"   // Convert camelCase to kebab-case
    | c when Char.IsNumber c -> ""                    // Omit characters that are digits
    | c when Regex.IsMatch(string c, @"[α-ω]") -> "?" // Replace Greek lower case letters with question marks
    | _ -> string c
    
let clean (identifier: string): string =
    identifier |> String.collect (fun (c: char) -> transform c)
