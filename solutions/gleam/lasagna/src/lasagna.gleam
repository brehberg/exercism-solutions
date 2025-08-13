// Define the expected oven time in minutes
pub fn expected_minutes_in_oven() -> Int {
    40
}

// Calculate the remaining oven time in minutes
pub fn remaining_minutes_in_oven(elapsed_bake_time: Int) -> Int {
    expected_minutes_in_oven() - elapsed_bake_time
}

// Calculate the preparation time in minutes
pub fn preparation_time_in_minutes(number_of_layers: Int) -> Int {
    let preparation_time_per_layer: Int = 2
    preparation_time_per_layer * number_of_layers
}

// Calculate the total working time in minutes
pub fn total_time_in_minutes(number_of_layers: Int, elapsed_bake_time: Int) -> Int {
    preparation_time_in_minutes(number_of_layers) + elapsed_bake_time
}

// Create a notification that the lasagna is ready
pub fn alarm() -> String {
    "Ding!"
}
