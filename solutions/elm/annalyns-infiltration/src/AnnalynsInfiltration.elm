module AnnalynsInfiltration exposing (canFastAttack, canFreePrisoner, canSignalPrisoner, canSpy, stealthAttackDamage)

-- Check if a fast attack can be made
canFastAttack : Bool -> Bool
canFastAttack knightIsAwake =
    not knightIsAwake

-- Check if the group can be spied upon
canSpy : Bool -> Bool -> Bool -> Bool
canSpy knightIsAwake archerIsAwake prisonerIsAwake =
    knightIsAwake || archerIsAwake || prisonerIsAwake

-- Check if the prisoner can be signalled
canSignalPrisoner : Bool -> Bool -> Bool
canSignalPrisoner archerIsAwake prisonerIsAwake =
    not archerIsAwake && prisonerIsAwake

-- Check if the prisoner can be freed
canFreePrisoner : Bool -> Bool -> Bool -> Bool -> Bool
canFreePrisoner knightIsAwake archerIsAwake prisonerIsAwake petDogIsPresent =
    if petDogIsPresent then not archerIsAwake
    else not (knightIsAwake || archerIsAwake) && prisonerIsAwake

-- Count the damage that a stealth attack could do
stealthAttackDamage : Bool -> Int
stealthAttackDamage annalynIsDetected =
    if annalynIsDetected then 7 else 12
