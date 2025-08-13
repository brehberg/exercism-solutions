class Attendee
  def initialize(height)
    @height = height
  end

  def issue_pass!(pass_id)
    @pass_id = pass_id
  end

  def revoke_pass!
    @pass_id = nil
  end

  # Do not edit above methods, add your own methods below.

  # Check if an attendee has a ride pass
  def has_pass?
    @pass_id || false
  end

  # Check if an attendee fits a ride
  def fits_ride?(ride_minimum_height)
    @height >= ride_minimum_height
  end

  # Check if an attendee is allowed to ride
  def allowed_to_ride?(ride_minimum_height)
    has_pass? && fits_ride?(ride_minimum_height)
  end
end
