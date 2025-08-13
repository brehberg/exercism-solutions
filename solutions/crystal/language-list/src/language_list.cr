module LanguageList
  # Define a method to return an empty language array
  def self.list : Array(String)
    [] of String
  end

  # Define a method to add a language to the array
  def self.add(list : Array(String), language : String) : Array(String)
    list << language
  end

  # Define a method to remove a language from the array
  def self.remove(list : Array(String)) : Array(String)
    list.pop
    list
  end

  # Define a method to return the nth item in the array
  def self.at(list : Array(String), index : Int) : String
    list[index]
  end

  # Define a method to parse a string of languages
  def self.parse(languages : String) : Array(String)
    languages.split(", ")
  end
end
