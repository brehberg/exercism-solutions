Function Get-BobResponse() {
    <#
    .SYNOPSIS
    Bob is a lackadaisical teenager. In conversation, his responses are very limited.
    
    .DESCRIPTION
    Bob is a lackadaisical teenager. In conversation, his responses are very limited.

    Bob answers 'Sure.' if you ask him a question.

    He answers 'Whoa, chill out!' if you yell at him.

    He answers 'Calm down, I know what I'm doing!' if you yell a question at him.

    He says 'Fine. Be that way!' if you address him without actually saying
    anything.

    He answers 'Whatever.' to anything else.
    
    .PARAMETER HeyBob
    The sentence you say to Bob.
    
    .EXAMPLE
    Get-BobResponse -HeyBob "Hi Bob"
    #>
    [CmdletBinding()]
    Param(
        [string]$HeyBob
    )

    [string] $questionReply = "Sure."
    [string] $yellingReply = "Whoa, chill out!"
    [string] $yellQuestionReply = "Calm down, I know what I'm doing!"
    [string] $silenceReply = "Fine. Be that way!"
    [string] $defaultReply = "Whatever."

    if ($HeyBob.Trim().Equals("")) {
        return $silenceReply
    }

    [boolean] $yelling = ($HeyBob -cmatch '[A-Z]' -and $HeyBob -cnotmatch '[a-z]')
    [boolean] $question = ($HeyBob -match '\?\s*$')

    return ($yelling) ?
        ($question) ? $yellQuestionReply : $yellingReply :
        ($question) ? $questionReply : $defaultReply    
}
