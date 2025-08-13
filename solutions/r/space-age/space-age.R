# Given an age in seconds, calculate how many years old
# someone that lived on the given planet would be
space_age <- function(seconds, planet) {
    orbital_period <- list(
        "mercury" = 0.2408467,
        "venus" = 0.61519726,
        "earth" = 1.0,
        "mars" = 1.8808158,
        "jupiter" = 11.862615,
        "saturn" = 29.447498,
        "uranus" = 84.016846,
        "neptune" = 164.79132
    )

    # 1.0 Earth year, 365.25 Earth days, 31557600 seconds
    round(seconds / 31557600 / orbital_period[[planet]], 2)
}
