# Given a person's allergy score, determine whether or not they're allergic
# to a given item, and their full list of allergies.
allergy <- function(num) {
    num
}

allergens <- list(
    "eggs" = 1,
    "peanuts" = 2,
    "shellfish" = 4,
    "strawberries" = 8,
    "tomatoes" = 16,
    "chocolate" = 32,
    "pollen" = 64,
    "cats" = 128
)

allergic_to <- function(allergy_object, allergy) {
    code <- allergens[allergy][[1]]
    bitwAnd(allergy_object, code) == code
}

list_allergies <- function(allergy_object) {
    out <- character()
    for (allergy in names(allergens)) {
        if (allergic_to(allergy_object, allergy)) {
            out <- c(out, allergy)
        }
    }
    out
}
