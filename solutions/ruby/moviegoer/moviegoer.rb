class Moviegoer
  def initialize(age, member: false)
    @age = age
    @member = member
  end

  # Check if a moviegoer is entitled to the seniors' discount
  def ticket_price
    @age < 60 ? 15 : 10
  end

  # Check if a moviegoer is allowed to see scary movies
  def watch_scary_movie?
    @age >= 18
  end

  # Check if a moviegoer is entitled to free popcorn
  def claim_free_popcorn!
    raise NotMovieClubMemberError.new unless  @member
    "🍿"
  end
end

# Custom exception used in Moviegoer code
class NotMovieClubMemberError < RuntimeError
end
