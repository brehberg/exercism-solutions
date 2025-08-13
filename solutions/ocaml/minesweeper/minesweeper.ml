let neighbors =
    [(-1, -1); (-1,  0); (-1,  1); (0,  1);
     ( 1,  1); ( 1,  0); ( 1, -1); (0, -1)]

let count_mines minefield (rows, cols) (r, c) =
    List.to_seq neighbors
    |> Seq.map (fun (r_offset, c_offset) -> (r + r_offset, c + c_offset))
    |> Seq.filter (fun (r, c) -> 0 <= r && r < rows && 0 <= c && c < cols)
    |> Seq.map (fun (r, c) -> if minefield.(r).[c] = '*' then 1 else 0)
    |> Seq.fold_left (+) 0

let annotate input =
    let minefield = Array.of_list input in
    let rows = Array.length minefield in
    let cols = match rows with | 0 -> 0 | _ -> String.length minefield.(0) in  
    let check_row r row =
        let count_mines c cell =
            if cell = ' ' then
                let count = count_mines minefield (rows, cols) (r, c)
                in 
                if count > 0 then count + Char.code '0' |> Char.chr else ' '
            else
                cell
        in 
        row |> String.mapi count_mines
    in 
    input |> List.mapi check_row
