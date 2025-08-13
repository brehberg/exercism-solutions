module Raindrops

let convert (number: int) : string =
    [ (3, "Pling"); (5, "Plang"); (7, "Plong") ]
    |> List.choose (fun (factor, sound) -> if number % factor = 0 then Some sound else None)
    |> function
        | [] -> string number
        | sounds -> String.concat "" sounds
