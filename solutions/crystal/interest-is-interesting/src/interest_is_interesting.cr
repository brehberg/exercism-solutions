class SavingsAccount
  # Calculate the interest rate
  def self.interest_rate(balance)
    if balance < 0
      3.213
    elsif balance < 1_000
      0.5
    elsif balance < 5_000
      1.621
    else
      2.475
    end
  end

  # Calculate the interest
  def self.interest(balance)
    balance * interest_rate(balance) / 100
  end

  # Calculate the annual balance update
  def self.annual_balance_update(balance)
    balance + interest(balance)
  end

  # Calculate the years before reaching the desired balance
  def self.years_before_desired_balance(current_balance, target_balance)
    years = 0
    while current_balance < target_balance
      years += 1
      current_balance = annual_balance_update(current_balance)
    end
    years
  end
end
