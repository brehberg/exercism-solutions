defmodule FreelancerRates do
  @moduledoc false
  @hours_per_day 8.0
  @days_per_month 22.0

  @doc "Calculate the daily rate given an hourly rate"
  @spec daily_rate(number) :: number
  def daily_rate(hourly_rate), do: @hours_per_day * hourly_rate

  @doc "Calculate a discounted price"
  @spec apply_discount(number, number) :: number
  def apply_discount(before_discount, discount),
    do: before_discount * (100 - discount) / 100

  @doc "Calculate the monthly rate, given an hourly rate and a discount"
  @spec monthly_rate(number, number) :: integer
  def monthly_rate(hourly_rate, discount) do
    hourly_rate
    |> daily_rate()
    |> Kernel.*(@days_per_month)
    |> apply_discount(discount)
    |> ceil()
  end

  @doc "Calculate the number of workdays given a budget, hourly rate and discount"
  @spec days_in_budget(number, number, number) :: float
  def days_in_budget(budget, hourly_rate, discount) do
    discount_rate =
      hourly_rate
      |> daily_rate()
      |> apply_discount(discount)

    Float.floor(budget / discount_rate, 1)
  end
end
