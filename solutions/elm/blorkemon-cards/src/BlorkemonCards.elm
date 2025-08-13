module BlorkemonCards exposing
    ( Card
    , compareShinyPower
    , expectedWinner
    , isMorePowerful
    , maxPower
    , sortByCoolness
    , sortByMonsterName
    )


type alias Card =
    { monster : String, power : Int, shiny : Bool }


isMorePowerful : Card -> Card -> Bool
isMorePowerful card1 card2 =
    card1.power > card2.power


maxPower : Card -> Card -> Int
maxPower card1 card2 =
    max card1.power card2.power


sortByMonsterName : List Card -> List Card
sortByMonsterName cards =
    cards |> List.sortBy .monster


sortByCoolness : List Card -> List Card
sortByCoolness cards =
    cards |> List.sortWith compareCoolFactor


compareCoolFactor : Card -> Card -> Order
compareCoolFactor card1 card2 =
    if card1.shiny then
        if card2.shiny then
            compare card2.power card1.power

        else
            LT

    else if card2.shiny then
        GT

    else if isMorePowerful card1 card2 then
        LT

    else if isMorePowerful card2 card1 then
        GT

    else
        EQ


compareShinyPower : Card -> Card -> Order
compareShinyPower card1 card2 =
    if isMorePowerful card1 card2 then
        GT

    else if isMorePowerful card2 card1 then
        LT

    else if card1.shiny then
        if card2.shiny then
            EQ

        else
            GT

    else if card2.shiny then
        LT

    else
        EQ


expectedWinner : Card -> Card -> String
expectedWinner card1 card2 =
    case compareShinyPower card1 card2 of
        GT ->
            card1.monster

        LT ->
            card2.monster

        EQ ->
            "too close to call"
