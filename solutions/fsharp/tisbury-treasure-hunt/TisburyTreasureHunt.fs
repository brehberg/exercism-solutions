module TisburyTreasureHunt

open System

let getCoordinate (line: string * string): string =
    snd line

let convertCoordinate (coordinate: string): int * char = 
    (Int32.Parse(string coordinate[0]), coordinate[1])

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let (_, ruisCoordinate, _) = ruisData
    azarasData |> getCoordinate |> convertCoordinate = ruisCoordinate

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =    
    if compareRecords azarasData ruisData then
        let (location, _, quadrant) = ruisData
        let (treasure, coordinate) = azarasData
        (coordinate, location, quadrant, treasure)
    else    
        ("", "", "", "")
