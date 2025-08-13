unit module SecretHandshake;

constant SECRET_ACTIONS = (
    0b0001 => "wink",
    0b0010 => "double blink",
    0b0100 => "close your eyes",
    0b1000 => "jump",
);

sub handshake ( $number ) is export {
    my @handshake = SECRET_ACTIONS.grep({ .key +& $number }).map: { .value };
    $number +& 0b10000 ?? @handshake.reverse.Array !! @handshake;
}
