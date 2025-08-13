local c_stop_codon = "STOP"
local c_mapping = {
    UGU = "Cysteine",
    UGC = "Cysteine",
    UUA = "Leucine",
    UUG = "Leucine",
    AUG = "Methionine",
    UUU = "Phenylalanine",
    UUC = "Phenylalanine",
    UCU = "Serine",
    UCC = "Serine",
    UCA = "Serine",
    UCG = "Serine",
    UGG = "Tryptophan",
    UAU = "Tyrosine",
    UAC = "Tyrosine",
    UAA = c_stop_codon,
    UAG = c_stop_codon,
    UGA = c_stop_codon
}

local function translate_codon(codon)
    return c_mapping[codon] or error("invalid codon")
end

local function translate_rna_strand(rna_strand)
    local result = {}
    for chunk in rna_strand:gmatch("...") do
        local amino_acid = translate_codon(chunk)
        if amino_acid == c_stop_codon then
            break
        end
        table.insert(result, amino_acid)
    end
    return result
end

return {codon = translate_codon, rna_strand = translate_rna_strand}
