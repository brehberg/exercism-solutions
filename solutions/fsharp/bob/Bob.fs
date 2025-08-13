module Bob

let response (input: string): string = 
    let defaultReply = "Whatever."
    let questionReply = "Sure."
    let shoutingReply = "Whoa, chill out!"
    let shoutingQuestionReply = "Calm down, I know what I'm doing!"
    let silenceReply = "Fine. Be that way!"
    
    let isShouting(): bool = 
        input = input.ToUpper() && input <> input.ToLower()

    let isQuestion(): bool =
        input.TrimEnd() |> Seq.last = '?'
    
    let isSilence(): bool =
        input.Trim() = ""

    if isSilence() then 
        silenceReply
    else match isShouting(), isQuestion() with
            | (true, true) -> shoutingQuestionReply
            | (true, false) -> shoutingReply
            | (false, true) -> questionReply
            | _ -> defaultReply