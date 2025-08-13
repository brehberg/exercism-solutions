Function Invoke-RnaTranscription() {
    <#
    .SYNOPSIS
    Transcribe a DNA strand into RNA.

    .DESCRIPTION
    Given a DNA strand, return its RNA complement (per RNA transcription).

    .PARAMETER Strand
    The DNA strand to transcribe.

    .EXAMPLE
    Invoke-RnaTranscription -Strand "A"
    #>
    [CmdletBinding()]
    Param(
        [string]$Strand
    )    
    if ($Strand -notmatch '^[ACGT]*$') { Throw 'Invalid nucleotide in strand' }
    $DnaToRna = @{ G = 'C'; C = 'G'; T = 'A'; A = 'U'; }

    [string] $Result = ""
    foreach ($Nucleotide in $Strand.ToCharArray()) {
        $Result += $DnaToRna[[string]$Nucleotide]
    }
    return $Result
}
