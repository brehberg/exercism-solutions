package Lasagna;

use v5.38;

# Define the expected oven time in minutes
our $ExpectedMinutesInOven = 40;

# Calculate the remaining oven time in minutes
sub remaining_minutes_in_oven ($actual_minutes_in_oven) {
    return $ExpectedMinutesInOven - $actual_minutes_in_oven;
}

# Calculate the preparation time in minutes
sub preparation_time_in_minutes ($number_of_layers) {
    our $PrepTimePerLayer = 2;
    return $PrepTimePerLayer * $number_of_layers;
}

# Calculate the total working time in minutes
sub total_time_in_minutes ( $number_of_layers, $actual_minutes_in_oven ) {
    return preparation_time_in_minutes($number_of_layers) + $actual_minutes_in_oven;
}

# Create a notification that the lasagna is ready
sub oven_alarm () {
    return "Ding!";
}
