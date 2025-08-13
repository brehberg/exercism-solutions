module SavingsAccount
  def self.interest_rate(balance)
    return 2.475 if balance >= 5000
    return 1.621 if balance >= 1000
    return 0.5 if balance >= 0
    return 3.213 # negative interest
  end

  def self.annual_balance_update(balance)
    return balance + balance * interest_rate(balance) / 100
  end

  def self.years_before_desired_balance(current_balance, desired_balance)
    years = 0
    while current_balance < desired_balance
      current_balance = annual_balance_update(current_balance)
      years += 1
    end
    return years
  end
end
