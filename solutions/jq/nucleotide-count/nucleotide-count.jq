if .strand|test("[^ACGT]") then
    "Invalid nucleotide in strand"|halt_error
else
    .strand
    | {A:0,C:0,G:0,T:0} as $counts
    | reduce (split("")|.[]) as $nuc ($counts; .[$nuc]+=1)
end
