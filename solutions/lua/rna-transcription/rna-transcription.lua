return function(dna)
    dna2rna = {G = "C", C = "G", T = "A", A = "U"}
    return dna:gsub("%S", dna2rna)
end
