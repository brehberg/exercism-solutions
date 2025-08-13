module SecretHandshake (handshake) where

import Data.Bits (testBit)

secretActions :: [(Int, [String] -> [String])]
secretActions =
  [ (4, reverse),
    (0, ("wink" :)),
    (1, ("double blink" :)),
    (2, ("close your eyes" :)),
    (3, ("jump" :))
  ]

handshake :: Int -> [String]
handshake n = foldr snd [] $ filter (testBit n . fst) secretActions
