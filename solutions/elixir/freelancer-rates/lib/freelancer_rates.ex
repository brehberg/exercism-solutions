defmodule FreelancerRates do
  @hours_per_day 8.0
  @days_per_month 22.0

  def daily_rate(hourly_rate), do: @hours_per_day * hourly_rate

  def apply_discount(before_discount, discount), do: before_discount * (100 - discount) / 100

  def monthly_rate(hourly_rate, discount) do
    before_discount = @days_per_month * daily_rate(hourly_rate)
    ceil(apply_discount(before_discount, discount))
  end

  def days_in_budget(budget, hourly_rate, discount) do
    discount_rate = apply_discount(daily_rate(hourly_rate), discount)
    Float.floor(budget / discount_rate, 1)
  end
end
