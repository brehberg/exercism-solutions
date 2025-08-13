unit module Scrabble;

my %letter-value = Hash.new:
    <AEIOULNRST DG BCMP FHVWY K JX QZ>.map(*.comb)
    «=>» <1 2 3 4 5 8 10>;

sub scrabble-score ($word) is export {
    $word.comb.map({%letter-value{.uc}}).sum;
}