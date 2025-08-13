class Attendee
  # Make new attendees
  def initialize(height)
    @height = height
  end

  # How tall is the attendee
  def height
    @height
  end

  # What is the ride pass id
  def pass_id
    @pass_id
  end

  # Allow people to buy a pass
  def issue_pass!(pass_id)
    @pass_id = pass_id
  end

  # Revoke the pass
  def revoke_pass!
    @pass_id = nil
  end
end
