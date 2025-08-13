module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    case name of
        Just playerName -> playerName
        Nothing -> "Mighty Magician"
        


revive : Player -> Maybe Player
revive player =
    case ( player.health, player.mana ) of        
        ( 0, Nothing ) -> Just { player | health = 100 }
        ( 0, _ ) -> Just { player | health = 100, mana = Just 100 }
        _ -> Nothing
        


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Just mana -> 
            if manaCost <= mana then
                ( { player | mana = Just (mana - manaCost) }, manaCost * 2)
            else
                ( player, 0)

        Nothing -> 
            if manaCost <= player.health then
                ( { player | health = player.health - manaCost }, 0)
            else
                ( { player | health = 0 }, 0)