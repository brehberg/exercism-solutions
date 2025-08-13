module Port
  IDENTIFIER = :PALE

  def self.get_identifier(city)
    city[0,4].upcase.to_sym
  end

  def self.get_terminal(ship_identifier)
    cargo = ship_identifier[0,3]
    if ["OIL", "GAS"].include? cargo then :A else :B end
  end
end
