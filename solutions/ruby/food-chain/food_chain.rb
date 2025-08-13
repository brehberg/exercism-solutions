class FoodChain
  def self.song
    0.upto(DEAD).collect { |i| verse(i) }.join("\n\n")
  end

  def self.verse(n)
    "I know an old lady who swallowed " +
      SWALLOWED[ANIMAL[n]] + "\n" +
      (n < DEAD ? to_catch(n) : "")
  end

  def self.to_catch(n)
    n.downto(0).collect { |i| WHY[ANIMAL[i]] }.join("\n")
  end

  SWALLOWED = {
    fly: "a fly.",
    spider: "a spider.\nIt wriggled and jiggled and tickled inside her.",
    bird: "a bird.\nHow absurd to swallow a bird!",
    cat: "a cat.\nImagine that, to swallow a cat!",
    dog: "a dog.\nWhat a hog, to swallow a dog!",
    goat: "a goat.\nJust opened her throat and swallowed a goat!",
    cow: "a cow.\nI don't know how she swallowed a cow!",
    horse: "a horse.\nShe's dead, of course!",
  }.freeze
  ANIMAL = SWALLOWED.keys.freeze
  WHY = {
    fly: "I don't know why she swallowed the fly. Perhaps she'll die.",
    spider: "She swallowed the spider to catch the fly.",
    bird: "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
    cat: "She swallowed the cat to catch the bird.",
    dog: "She swallowed the dog to catch the cat.",
    goat: "She swallowed the goat to catch the dog.",
    cow: "She swallowed the cow to catch the goat.",
  }.freeze
  DEAD = WHY.length.freeze
  private_constant :SWALLOWED, :ANIMAL, :WHY, :DEAD
  private_class_method :verse, :to_catch
end
