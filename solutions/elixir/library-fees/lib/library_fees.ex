defmodule LibraryFees do
  @doc """
  Parse the stored datetimes
  """
  @spec datetime_from_string(String.t()) :: NaiveDateTime.t()
  def datetime_from_string(string),
    do: NaiveDateTime.from_iso8601!(string)

  @doc """
  Determine if a book was checked out before noon
  """
  @spec before_noon?(NaiveDateTime.t()) :: boolean
  def before_noon?(datetime),
    do: Time.compare(datetime, ~T[12:00:00]) == :lt

  @doc """
  Calculate the return date
  """
  @spec return_date(NaiveDateTime.t()) :: Date.t()
  def return_date(checkout_datetime),
    do:
      if(before_noon?(checkout_datetime),
        do: add_days(checkout_datetime, 28),
        else: add_days(checkout_datetime, 29)
      )

  @seconds_per_day 24 * 60 * 60
  @spec add_days(NaiveDateTime.t(), integer) :: Date.t()
  defp add_days(datetime, days),
    do:
      datetime
      |> NaiveDateTime.add(days * @seconds_per_day)
      |> NaiveDateTime.to_date()

  @doc """
  Determine how late the return of the book was
  """
  @spec days_late(Date.t(), NaiveDateTime.t()) :: non_neg_integer
  def days_late(planned_return_date, actual_return_datetime),
    do:
      actual_return_datetime
      |> Date.diff(planned_return_date)
      |> max(0)

  @doc """
  Determine if the book was returned on a Monday
  """
  @spec monday?(NaiveDateTime.t()) :: boolean
  def monday?(datetime),
    do: Date.day_of_week(datetime) == 1

  @doc """
  Calculate the late fee
  """
  @spec calculate_late_fee(String.t(), String.t(), integer) :: integer
  def calculate_late_fee(checkout, return, rate) do
    checkout_datetime = datetime_from_string(checkout)
    planned_return_date = return_date(checkout_datetime)
    actual_return_datetime = datetime_from_string(return)
    late_fee = days_late(planned_return_date, actual_return_datetime) * rate
    if monday?(actual_return_datetime), do: div(late_fee, 2), else: late_fee
  end
end
