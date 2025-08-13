.strand
| if test("[^ACGT]") then
    "Invalid nucleotide in strand"|halt_error
  else
    reduce split("")[] as $n ({A:0,C:0,G:0,T:0}; .[$n]+=1)
  end
