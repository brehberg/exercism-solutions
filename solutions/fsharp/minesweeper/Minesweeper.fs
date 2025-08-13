module Minesweeper

let annotate (input: string list) =
    // transform input list of strings into 2D matrix
    let output = array2D input
    let rows = output.GetLength 0
    let cols = output.GetLength 1
    
    // create 2D matrix with zero-filled rows and cols around the outer edge
    let minefield = Array2D.zeroCreateBased -1 -1 (rows + 2) (cols + 2)
    Array2D.blit output 0 0 minefield 0 0 rows cols

    // traverse the output to find empty cell 
    output
    |> Array2D.iteri (fun row col value ->
        if value = ' ' then
            let mutable mines = 0
            // check all neighbors for this cell and count mines around it
            minefield.[row-1..row+1, col-1..col+1]
            |> Array2D.iter (fun x -> if x = '*' then mines <- mines + 1)
            // replace output with the character value for number of mines
            if mines > 0 then output.[row, col] <- (string mines)[0] )
    
    // transform output 2D matrix back into a list of strings
    [ for i in 0..rows-1 do System.String.Concat output.[i,*] ]
