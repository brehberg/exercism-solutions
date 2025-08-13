module TisburyTreasureHunt

let getCoordinate (line: string * string): string = snd line

let convertCoordinate (coordinate: string): int * char = 
    coordinate[0] |> string |> int, coordinate[1]

let compareRecords (azarasData: string * string) (ruisData: string * (int * char) * string) : bool = 
    let (_, ruisCoordinate, _) = ruisData
    azarasData |> getCoordinate |> convertCoordinate = ruisCoordinate

let createRecord (azarasData: string * string) (ruisData: string * (int * char) * string) : (string * string * string * string) =    
    if compareRecords azarasData ruisData then
        match azarasData, ruisData with
        | (treasure, coordinate), (location, _, quadrant) -> (coordinate, location, quadrant, treasure)
    else    
        ("", "", "", "")
