local lasagna = {}

-- Define the expected oven time in minutes
lasagna["oven_time"] = 40

-- Calculate the remaining oven time in minutes
function lasagna.remaining_oven_time(actual_minutes_in_oven)
  return lasagna["oven_time"] - actual_minutes_in_oven
end

-- Calculate the preparation time in minutes
function lasagna.preparation_time(number_of_layers)
  local time_per_layer = 2
  return time_per_layer * number_of_layers
end

-- Calculate the elapsed working time in minutes
function lasagna.elapsed_time(number_of_layers, actual_minutes_in_oven)
  return lasagna.preparation_time(number_of_layers) + actual_minutes_in_oven
end

return lasagna
