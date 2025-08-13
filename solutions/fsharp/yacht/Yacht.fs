module Yacht

type Category = 
    | Ones
    | Twos
    | Threes
    | Fours
    | Fives
    | Sixes
    | FullHouse
    | FourOfAKind
    | LittleStraight
    | BigStraight
    | Choice
    | Yacht

type Die =
    | One = 1
    | Two = 2
    | Three = 3
    | Four = 4
    | Five = 5
    | Six = 6


let private sumForValue (value: int) (dice: int list): int =
    dice |> List.filter(fun die -> die = value) |> List.sum

let private checkYacht (dice: int list): int =
    if dice |> List.distinct |> List.length = 1 then 50 else 0

let private checkStraight (values: int list) (dice: int list): int =
    if dice |> List.sort = values then 30 else 0

let private checkFourOfAKind (dice: int list): int =
    match dice |> List.sort with
    | [d1; d2; d3; d4; d5] -> if (d1 = d4 || d2 = d5) then d3 * 4 else 0
    | _ -> 0

let private checkFullHouse (dice: int list): int =
    match dice |> List.countBy id with
    | [(_, 2); (_, 3)] -> dice |> List.sum
    | _ -> 0


let score (category: Category) (dice: Die list) = 
    dice |> List.map int |>
    match category with
    | Ones           -> sumForValue 1
    | Twos           -> sumForValue 2
    | Threes         -> sumForValue 3
    | Fours          -> sumForValue 4
    | Fives          -> sumForValue 5
    | Sixes          -> sumForValue 6
    | FullHouse      -> checkFullHouse
    | FourOfAKind    -> checkFourOfAKind
    | LittleStraight -> checkStraight [1; 2; 3; 4; 5]
    | BigStraight    -> checkStraight [2; 3; 4; 5; 6]
    | Yacht          -> checkYacht
    | Choice         -> List.sum