module TwoBucket exposing (BucketNumber(..), measure)


type BucketNumber
    = One
    | Two


type alias State =
    { moves : Int, bucketOne : Int, bucketTwo : Int }


measure : Int -> Int -> Int -> BucketNumber -> Maybe State
measure bucketOneSize bucketTwoSize goal startBucket =
    if not (isValid bucketOneSize bucketTwoSize goal) then
        Nothing

    else
        let
            one =
                Bucket One bucketOneSize 0

            two =
                Bucket Two bucketTwoSize 0

            ( first, second ) =
                case startBucket of
                    One ->
                        ( one, two )

                    Two ->
                        ( two, one )
        in
        if second.size == goal && first.size /= goal then
            Just (solve (fill first) (fill second) goal 2)

        else
            Just (solve (fill first) second goal 1)


solve : Bucket -> Bucket -> Int -> Int -> State
solve first second goal moves =
    if first.amount == goal || second.amount == goal then
        case first.name of
            One ->
                State moves first.amount second.amount

            Two ->
                State moves second.amount first.amount

    else if isEmpty first then
        solve (fill first) second goal (moves + 1)

    else if isFull second then
        solve first (empty second) goal (moves + 1)

    else
        let
            ( a, b ) =
                pour first second
        in
        solve a b goal (moves + 1)



-- validation functions


gcd : Int -> Int -> Int
gcd a b =
    if b == 0 then
        a

    else
        gcd b (modBy b a)


isValid : Int -> Int -> Int -> Bool
isValid aSize bSize goal =
    let
        factor =
            gcd aSize bSize
    in
    goal <= max aSize bSize && (factor == 1 || modBy factor goal == 0)



-- bucket type and helper functions


type alias Bucket =
    { name : BucketNumber
    , size : Int
    , amount : Int
    }


isFull : Bucket -> Bool
isFull b =
    b.amount == b.size


isEmpty : Bucket -> Bool
isEmpty b =
    b.amount == 0


fill : Bucket -> Bucket
fill b =
    { b | amount = b.size }


empty : Bucket -> Bucket
empty b =
    { b | amount = 0 }


pour : Bucket -> Bucket -> ( Bucket, Bucket )
pour from to =
    let
        quantity =
            min from.amount (to.size - to.amount)
    in
    ( { from | amount = from.amount - quantity }
    , { to | amount = to.amount + quantity }
    )
