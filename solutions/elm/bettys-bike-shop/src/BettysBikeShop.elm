module BettysBikeShop exposing (poundsToString, penceToPounds)

import String exposing (fromFloat)

-- Convert pence to pounds
penceToPounds : Int -> Float
penceToPounds pence =
    toFloat pence / 100

-- Format the price for display on the website
poundsToString : Float -> String
poundsToString pounds =
    "£" ++ fromFloat pounds
