Function Invoke-ProteinTranslation() {
    <#
    .SYNOPSIS
    Translate RNA sequences into proteins.

    .DESCRIPTION
    Take an RNA sequence and convert it into condons and then into the name of the proteins in the form of a list.

    .PARAMETER Strand
    The RNA sequence to translate.

    .EXAMPLE
    Invoke-ProteinTranslation -Strand "AUG"
    #>
    [CmdletBinding()]
    Param(
        [string]$Strand
    )
    
    Set-Variable STOP_CODON -option Constant -value "STOP"

    function Invoke-CodonTranslation {
        Param( 
            [string]$Codon
        )
        Switch ($Codon) {
            { @("AUG") -contains $_ } { "Methionine" }
            { @("UGG") -contains $_ } { "Tryptophan" }
            { @("UGU" , "UGC") -contains $_ } { "Cysteine" }
            { @("UUA" , "UUG") -contains $_ } { "Leucine" }
            { @("UUU" , "UUC") -contains $_ } { "Phenylalanine" }            
            { @("UAU" , "UAC") -contains $_ } { "Tyrosine" }        
            { @("UCU" , "UCC" , "UCA" , "UCG") -contains $_ } { "Serine" }
            { @("UAA" , "UAG" , "UGA") -contains $_ } { $STOP_CODON }
            default { Throw "error: Invalid codon" }
        }           
    }    

    $result = New-Object System.Collections.Generic.List[string]
    ForEach ($chunk in $Strand -split '(.{3})' -ne '') {
        [string]$amino_acid = Invoke-CodonTranslation $chunk
        if ($amino_acid -eq $STOP_CODON) {
            Break
        }
        $result.Add($amino_acid)
    }
    Return $result.ToArray()
}
