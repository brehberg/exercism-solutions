defmodule BirdCount do
  @moduledoc false

  @doc "Check how many birds visited today"
  @spec today([integer]) :: integer
  def today([]), do: nil
  def today([today | _]), do: today

  @doc "Increment today's count"
  @spec increment_day_count([integer]) :: [integer]
  def increment_day_count([]), do: [1]
  def increment_day_count([today | rest]), do: [today + 1 | rest]

  @doc "Check if there was a day with no visiting birds"
  @spec has_day_without_birds?([integer]) :: boolean
  def has_day_without_birds?([]), do: false
  def has_day_without_birds?([0 | _]), do: true
  def has_day_without_birds?([_ | rest]), do: has_day_without_birds?(rest)

  @doc "Calculate the total number of visiting birds"
  @spec total([integer], integer) :: integer
  def total(list, sum \\ 0)
  def total([], sum), do: sum
  def total([today | rest], sum), do: total(rest, sum + today)

  @doc "Calculate the number of busy days"
  @spec busy_days([integer]) :: integer
  def busy_days(list, count \\ 0)
  def busy_days([], count), do: count
  def busy_days([today | rest], count) when today >= 5, do: busy_days(rest, count + 1)
  def busy_days([_ | rest], count), do: busy_days(rest, count)
end
