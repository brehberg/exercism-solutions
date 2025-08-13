unit module Luhn;

multi is-luhn-valid ($input --> Bool) is export {
    with $input.comb(/<:Number>/) {
        return False if .elems < 2;
        .reverse >>*>> (1,2) andthen .map(*.comb.sum).sum %% 10
    }
}
multi is-luhn-valid ($input where /<:!Number - :Space_Separator>/ --> False) is export {}