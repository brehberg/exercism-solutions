module CircularBuffer exposing (CircularBuffer, clear, new, overwrite, read, write)

import Array exposing (Array)


type CircularBuffer a
    = CircularBuffer Int (Array a)


new : Int -> CircularBuffer a
new size =
    CircularBuffer size Array.empty


write : a -> CircularBuffer a -> Maybe (CircularBuffer a)
write element ((CircularBuffer size buffer) as circular) =
    if Array.length buffer == size then
        Nothing

    else
        circular |> push element |> Just


overwrite : a -> CircularBuffer a -> CircularBuffer a
overwrite element ((CircularBuffer size buffer) as circular) =
    if Array.length buffer == size then
        circular |> clear |> push element

    else
        circular |> push element


push : a -> CircularBuffer a -> CircularBuffer a
push element (CircularBuffer size buffer) =
    Array.push element buffer
        |> CircularBuffer size


read : CircularBuffer a -> Maybe ( a, CircularBuffer a )
read ((CircularBuffer _ buffer) as circular) =
    Array.get 0 buffer
        |> Maybe.map (\value -> ( value, clear circular ))


clear : CircularBuffer a -> CircularBuffer a
clear (CircularBuffer size buffer) =
    Array.slice 1 (Array.length buffer) buffer
        |> CircularBuffer size
