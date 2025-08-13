module TisburyTreasureHunt exposing (..)


type alias TreasureLocation =
    ( Int, Char )


type alias Treasure =
    ( String, TreasureLocation )


type alias PlaceLocation =
    ( Char, Int )


type alias Place =
    ( String, PlaceLocation )


placeLocationToTreasureLocation : PlaceLocation -> TreasureLocation
placeLocationToTreasureLocation ( letter, number ) =
    ( number, letter )


treasureLocationMatchesPlaceLocation : PlaceLocation -> TreasureLocation -> Bool
treasureLocationMatchesPlaceLocation placeLocation treasureLocation =
    treasureLocation == placeLocationToTreasureLocation placeLocation


countPlaceTreasures : Place -> List Treasure -> Int
countPlaceTreasures ( _, placeLocation ) treasures =
    treasures
        |> List.filter (Tuple.second >> treasureLocationMatchesPlaceLocation placeLocation)
        |> List.length


specialCaseSwapPossible : Treasure -> Place -> Treasure -> Bool
specialCaseSwapPossible ( foundTreasure, _ ) ( place, _ ) ( desiredTreasure, _ ) =
    case ( foundTreasure, place ) of
        -- The Brass Spyglass can be swapped for any other treasure at the Abandoned Lighthouse
        ( "Brass Spyglass", "Abandoned Lighthouse" ) ->
            True

        -- The Amethyst Octopus can be swapped for the Crystal Crab or the Glass Starfish at the Stormy Breakwater
        ( "Amethyst Octopus", "Stormy Breakwater" ) ->
            desiredTreasure == "Crystal Crab" || desiredTreasure == "Glass Starfish"

        -- The Vintage Pirate Hat can be swapped for the Model Ship in Large Bottle or the Antique Glass Fishnet Float at the Harbor Managers Office
        ( "Vintage Pirate Hat", "Harbor Managers Office" ) ->
            desiredTreasure == "Model Ship in Large Bottle" || desiredTreasure == "Antique Glass Fishnet Float"

        _ ->
            False
