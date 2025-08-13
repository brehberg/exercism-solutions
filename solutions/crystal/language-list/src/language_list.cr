module LanguageList
  alias LangList = Array(String)

  # Define a method to return an empty language array
  def self.list : LangList
    [] of String
  end

  # Define a method to add a language to the array
  def self.add(list : LangList, language : String) : LangList
    list << language
  end

  # Define a method to remove a language from the array
  def self.remove(list : LangList) : LangList
    list.pop
    list
  end

  # Define a method to return the nth item in the array
  def self.at(list : LangList, index : Int) : String
    list[index]
  end

  # Define a method to parse a string of languages
  def self.parse(languages : String) : LangList
    languages.split(", ")
  end
end
