class FoodChain
  def self.song
    0.upto(DEAD).collect { |i| verse(i) }.join("\n\n")
  end

  def self.verse(n)
    SWALLOWED[ANIMAL[n]] + "\n" << (n < DEAD ? to_catch(n) : "")
  end

  def self.to_catch(n)
    n.downto(0).collect { |i| WHY[ANIMAL[i]] }.join("\n")
  end

  ANIMAL = [:fly, :spider, :bird, :cat, :dog, :goat, :cow, :horse].freeze
  SWALLOWED = {
    fly: "I know an old lady who swallowed a fly.",
    spider: "I know an old lady who swallowed a spider.\nIt wriggled and jiggled and tickled inside her.",
    bird: "I know an old lady who swallowed a bird.\nHow absurd to swallow a bird!",
    cat: "I know an old lady who swallowed a cat.\nImagine that, to swallow a cat!",
    dog: "I know an old lady who swallowed a dog.\nWhat a hog, to swallow a dog!",
    goat: "I know an old lady who swallowed a goat.\nJust opened her throat and swallowed a goat!",
    cow: "I know an old lady who swallowed a cow.\nI don't know how she swallowed a cow!",
    horse: "I know an old lady who swallowed a horse.\nShe's dead, of course!",
  }.freeze
  WHY = {
    fly: "I don't know why she swallowed the fly. Perhaps she'll die.",
    spider: "She swallowed the spider to catch the fly.",
    bird: "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
    cat: "She swallowed the cat to catch the bird.",
    dog: "She swallowed the dog to catch the cat.",
    goat: "She swallowed the goat to catch the dog.",
    cow: "She swallowed the cow to catch the goat.",
  }.freeze
  DEAD = (ANIMAL.length - 1).freeze
  private_constant :ANIMAL, :SWALLOWED, :WHY, :DEAD
end
