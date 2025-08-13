module MonsterAttack exposing (..)


type alias MonsterDamage =
    String


attackWithSword1 : MonsterDamage -> Int -> MonsterDamage
attackWithSword1 monsterDamage strength =
    String.concat
        [ monsterDamage
        , "Attacked with sword of strength "
        , strength |> String.fromInt
        , "."
        ]


attackWithClaw1 : MonsterDamage -> Int -> MonsterDamage
attackWithClaw1 monsterDamage strength =
    String.concat
        [ monsterDamage
        , "Attacked with claw of strength "
        , strength |> String.fromInt
        , "."
        ]


attack1 : MonsterDamage -> MonsterDamage
attack1 monsterDamage =
    attackWithSword1
        (attackWithClaw1
            (attackWithClaw1
                (attackWithSword1
                    monsterDamage
                    5
                )
                1
            )
            1
        )
        5


attackWithSword2 : Int -> MonsterDamage -> MonsterDamage
attackWithSword2 strength monsterDamage =
    String.concat
        [ monsterDamage
        , "Attacked with sword of strength "
        , strength |> String.fromInt
        , "."
        ]


attackWithClaw2 : Int -> MonsterDamage -> MonsterDamage
attackWithClaw2 strength monsterDamage =
    String.concat
        [ monsterDamage
        , "Attacked with claw of strength "
        , strength |> String.fromInt
        , "."
        ]


attack2 : MonsterDamage -> MonsterDamage
attack2 monsterDamage =
    let
        annalynAttack =
            attackWithSword2 5

        kazakAttack =
            attackWithClaw2 1
    in
    monsterDamage
        |> annalynAttack
        |> kazakAttack
        |> kazakAttack
        |> annalynAttack


attack3 : MonsterDamage -> MonsterDamage
attack3 =
    let
        annalynAttack =
            attackWithSword2 5

        kazakAttack =
            attackWithClaw2 1
    in
    annalynAttack
        << kazakAttack
        << kazakAttack
        << annalynAttack
