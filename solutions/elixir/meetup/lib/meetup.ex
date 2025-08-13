defmodule Meetup do
  @moduledoc """
  Calculate meetup dates.
  """

  @type weekday ::
          :monday
          | :tuesday
          | :wednesday
          | :thursday
          | :friday
          | :saturday
          | :sunday

  @type schedule :: :first | :second | :third | :fourth | :last | :teenth

  @initial_day %{
    :teenth => 13,
    :first => 1,
    :second => 8,
    :third => 15,
    :fourth => 22
  }

  @doc """
  Calculate a meetup date.

  The schedule is in which week (1..4, last or "teenth") the date should fall.

  ## Examples:

    the third Tuesday of August 2019 (August 20, 2019)
      iex>Meetup.meetup(2019, 8, :tuesday, :third)
      ~D[2019-08-20]

    the teenth Wednesday of May 2020 (May 13, 2020)
      iex>Meetup.meetup(2020, 5, :wednesday, :teenth)
      ~D[2020-05-13]

    the fourth Sunday of July 2021 (July 25, 2021)
      iex>Meetup.meetup(2021, 7, :sunday, :fourth)
      ~D[2021-07-25]

    the last Thursday of November 2022 (November 24, 2022)
      iex>Meetup.meetup(2022, 11, :thursday, :last)
      ~D[2022-11-24]

  """
  @spec meetup(pos_integer, pos_integer, weekday, schedule) :: Date.t()
  def meetup(year, month, weekday, :last) do
    guess = Date.new!(year, month, 1) |> Date.end_of_month()

    case Date.day_of_week(guess, weekday) do
      1 -> guess
      n -> Date.add(guess, 1 - n)
    end
  end

  def meetup(year, month, weekday, schedule) do
    guess = Date.new!(year, month, @initial_day[schedule])

    case Date.day_of_week(guess, weekday) do
      1 -> guess
      n -> Date.add(guess, 8 - n)
    end
  end
end
