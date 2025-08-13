module ReverseString

let reverse (input: string) : string =
    let sb = new System.Text.StringBuilder()

    for c in Seq.rev input do
        sb.Append(c) |> ignore

    sb.ToString()
