module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    case
        game
            |> oneStonePerPointRule
            |> Result.map captureRule
            |> Result.andThen libertyRule
            |> Result.andThen koRule
    of
        Err errorMsg ->
            { game | error = errorMsg }

        Ok validGame ->
            validGame |> changePlayer
