class LocomotiveEngineer
  # Create a list of all wagons
  def self.generate_list_of_wagons(*wagons_ids)
    wagons_ids
  end

  # Fix the list of wagons
  def self.fix_list_of_wagons(each_wagons_id, missing_wagons)
    car1, car2, loco, *rest = each_wagons_id
    [loco, *missing_wagons, *rest, car1, car2]
  end

  # Add missing stops
  def self.add_missing_stops(route, **stops)
    { **route, stops: stops.map { |_, stop| stop } }
  end

  # Extend routing information
  def self.extend_route_information(route, more_route_information)
    { **route, **more_route_information }
  end
end
