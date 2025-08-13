module RunLengthEncoding exposing (decode, encode)

import Regex



-- Encode returns a string where consecutive elements are represented as a count and value.


encode : String -> String
encode string =
    Regex.replace chunks encodedChunk string


chunks : Regex.Regex
chunks =
    Regex.fromString "(.)\\1+" |> Maybe.withDefault Regex.never


encodedChunk : Regex.Match -> String
encodedChunk m =
    let
        char =
            m.submatches |> List.head |> Maybe.withDefault Nothing |> Maybe.withDefault ""

        count =
            m.match |> String.length |> String.fromInt
    in
    count ++ char



-- Decode returns a string that has been reconstructed from the input into its original form.


decode : String -> String
decode string =
    Regex.replace groups decodedGroup string


groups : Regex.Regex
groups =
    Regex.fromString "(\\d+)(.)" |> Maybe.withDefault Regex.never


decodedGroup : Regex.Match -> String
decodedGroup m =
    let
        char =
            m.submatches |> List.drop 1 |> List.head |> Maybe.withDefault Nothing |> Maybe.withDefault ""

        count =
            m.submatches |> List.head |> Maybe.withDefault Nothing |> Maybe.withDefault "" |> String.toInt |> Maybe.withDefault 0
    in
    String.repeat count char
