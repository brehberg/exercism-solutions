#  Determine the actions of a secret handshake based on the binary
#  representation of the given `code`.
handshake <- function(code) {
    secret <- function(actions, num, action) {
        if (bitwAnd(code, num) == num) {
            if (action == "reverse") {
                return(rev(actions))
            }
            return(c(actions, action))
        }
        actions
    }

    #  If the following bits are set, include the corresponding action in
    #  your list of commands, in order from lowest to highest.
    #    0b1 = wink
    #    0b10 = double blink
    #    0b100 = close your eyes
    #    0b1000 = jump
    #
    #    0b10000 = Reverse the order of the operations in the secret handshake
    (c() |> secret(0x1, "wink")
        |> secret(0x02, "double blink")
        |> secret(0x04, "close your eyes")
        |> secret(0x08, "jump")
        |> secret(0x10, "reverse"))
}
