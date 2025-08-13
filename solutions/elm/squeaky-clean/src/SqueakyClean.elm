module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)

import Json.Decode exposing (value)
import Regex



-- Replace spaces with underscores


clean1 : String -> String
clean1 str =
    str |> String.replace " " "_"



--Replace control characters with the upper case string "[CTRL]"


controlChars : Regex.Regex
controlChars =
    "[\n\t\u{000D}]" |> Regex.fromString |> Maybe.withDefault Regex.never


clean2 : String -> String
clean2 str =
    str |> clean1 |> Regex.replace controlChars (\_ -> "[CTRL]")



-- Convert kebab-case to camelCase


matchKebabCase : Regex.Regex
matchKebabCase =
    "-(.)" |> Regex.fromString |> Maybe.withDefault Regex.never


convertToUpperCase : Regex.Match -> String
convertToUpperCase { submatches } =
    case submatches of
        [ Just value ] ->
            value |> String.toUpper

        _ ->
            ""


clean3 : String -> String
clean3 str =
    str |> clean2 |> Regex.replace matchKebabCase convertToUpperCase



-- Omit digits


clean4 : String -> String
clean4 str =
    str |> clean3 |> String.filter (Char.isDigit >> not)



-- Omit Greek lower case letters


clean : String -> String
clean str =
    str |> clean4 |> String.filter (\c -> c < 'α' || c > 'ω')
