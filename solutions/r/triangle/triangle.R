type <- list(
    nat = c("not a triangle"),
    equ = c("equilateral", "isosceles"),
    iso = c("isosceles"),
    sca = c("scalene")
)

# triangle determines if a triangle is equilateral, isosceles, or scalene.
triangle <- function(x, y, z) {
    if (has_invalid_side(x, y, z) ||
        has_invalid_length(x, y, z)) {
        stop(type$nat)
    }

    if (x == y && y == z) {
        structure(c(x, y, z), class = type$equ)
    } else if (x == y || y == z || x == z) {
        structure(c(x, y, z), class = type$iso)
    } else {
        structure(c(x, y, z), class = type$sca)
    }
}

# all side lenghts must be positive
has_invalid_side <- function(x, y, z) {
    x <= 0 || y <= 0 || z <= 0
}

# side lengths cannot violate triangle inequality
has_invalid_length <- function(x, y, z) {
    x + y < z || y + z < x || x + z < y
}
