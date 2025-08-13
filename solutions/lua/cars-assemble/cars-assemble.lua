local cars = {}

-- returns the amount of working cars produced by the assembly line every hour
function cars.calculate_working_cars_per_hour(production_rate, success_rate)
  return production_rate * success_rate / 100
end

-- returns the amount of working cars produced by the assembly line every minute
function cars.calculate_working_cars_per_minute(production_rate, success_rate)
  return cars.calculate_working_cars_per_hour(production_rate, success_rate) // 60
end

-- returns the cost of producing the given number of cars
function cars.calculate_cost(cars_count)
  local cost_for_one = 10000
  local cost_for_ten = 95000
  local group_of_ten_cost = cars_count // 10 * cost_for_ten
  local individual_cost = cars_count % 10 * cost_for_one
  return group_of_ten_cost + individual_cost
end

return cars
