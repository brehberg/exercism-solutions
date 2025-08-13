module Go exposing (..)

import GoSupport exposing (..)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    game
        |> oneStonePerPointRule
        |> Result.map captureRule
        |> Result.andThen libertyRule
        |> Result.andThen koRule
        |> checkResult game


checkResult : Game -> Result String Game -> Game
checkResult game result =
    case result of
        Err error ->
            { game | error = error }

        Ok newGame ->
            changePlayer newGame
