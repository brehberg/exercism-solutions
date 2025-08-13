def validate:
  if .|test("[^ACGT]") then "Invalid nucleotide in strand"|halt_error end;

.strand
| validate
| reduce split("")[] as $n ({A:0,C:0,G:0,T:0}; .[$n]+=1)
