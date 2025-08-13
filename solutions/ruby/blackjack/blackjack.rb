module Blackjack
  # Calculate the value of any given card
  def self.parse_card(card)
    case card
    when "ace"
      11
    when "king", "queen", "jack", "ten"
      10
    when "nine"
      9
    when "eight"
      8
    when "seven"
      7
    when "six"
      6
    when "five"
      5
    when "four"
      4
    when "three"
      3
    when "two"
      2
    else
      0
    end
  end

  # Name ranges of values.
  def self.card_range(card1, card2)
    sum = parse_card(card1) + parse_card(card2)
    case
    when sum == 21
      "blackjack"
    when sum >= 17
      "high"
    when sum >= 12
      "mid"
    else
      "low"
    end
  end

  # Implement the decision logic for the first turn.
  def self.first_turn(card1, card2, dealer_card)
    hand = card_range(card1, card2)
    case
    when card1 == "ace" && card2 == "ace"
      "P" # Split
    when hand == "blackjack" && parse_card(dealer_card) < 10
      "W" # auto win
    when hand == "low" || hand == "mid" && parse_card(dealer_card) >= 7
      "H" # Hit
    else
      "S" # Stand
    end
  end
end
