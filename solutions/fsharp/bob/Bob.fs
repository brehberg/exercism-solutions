module Bob

open System

let response (input: string): string = 
    let defaultReply = "Whatever."
    let questionReply = "Sure."
    let shoutingReply = "Whoa, chill out!"
    let shoutingQuestionReply = "Calm down, I know what I'm doing!"
    let silenceReply = "Fine. Be that way!"
    
    let isShouting(): bool = 
        input = input.ToUpper() && input <> input.ToLower()

    let isQuestion(): bool =
        input.TrimEnd().EndsWith('?')
    
    let isSilence(): bool =
        String.IsNullOrWhiteSpace(input)

    if isSilence() then 
        silenceReply
    else match isShouting(), isQuestion() with
            | (true, true) -> shoutingQuestionReply
            | (true, _) -> shoutingReply
            | (_, true) -> questionReply
            | _ -> defaultReply