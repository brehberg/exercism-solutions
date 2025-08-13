module TreasureChest exposing (..)


type TreasureChest treasure
    = TreasureChest String treasure


getTreasure : String -> TreasureChest treasure -> Maybe treasure
getTreasure passwordAttempt (TreasureChest password treasure) =
    if password == passwordAttempt then
        Just treasure

    else
        Nothing


multiplyTreasure :
    (treasure -> List treasure)
    -> TreasureChest treasure
    -> TreasureChest (List treasure)
multiplyTreasure multiplier (TreasureChest password treasure) =
    TreasureChest password (multiplier treasure)
