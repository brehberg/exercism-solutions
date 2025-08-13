module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name
        


revive : Player -> Maybe Player
revive ({ health, mana } as player) =
    case ( health, mana ) of
        ( 0, Nothing ) -> Just { player | health = 100 }
        ( 0, _ ) -> Just { player | health = 100, mana = Just 100 }
        _ -> Nothing
        


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Just mana ->
            if manaCost > mana then
                ( player, 0 )
            else
                ( { player | mana = Just (mana - manaCost) }, manaCost * 2 )

        Nothing -> 
            ( { player | health = max (player.health - manaCost) 0 }, 0 )
