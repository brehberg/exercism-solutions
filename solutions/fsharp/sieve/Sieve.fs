module Sieve

let primes limit =
    let rec sieve numbers primes =
        match numbers with
        | [] -> List.rev primes
        | prime :: rest ->
            let multiples = [ prime * prime .. 2 * prime .. limit ]
            sieve (rest |> List.except multiples) (prime :: primes)

    match limit < 2 with
    | true -> []
    | _ -> sieve [ 3..2..limit ] [ 2 ]
