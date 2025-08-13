class PartyRobot
  # Welcome a new guest to the party
  def self.welcome(name : String) : String
    "Welcome to my party, #{name}!"
  end

  # Welcome a new guest to the party whose birthday is today
  def self.happy_birthday(name : String, age : Int) : String
    "Happy birthday #{name}! You are now #{age} years old!"
  end

  # Give directions
  def self.assign_table(name : String, direction : String, table : Int, distance : Float64, neighbor : String) : String
    welcome(name) +
      "\nYou have been assigned to table #{table}. " +
      "Your table is #{direction}, exactly #{distance.round(1)} meters from here." +
      "\nYou will be sitting next to #{neighbor[0]}#{neighbor[-1]}."
  end
end
