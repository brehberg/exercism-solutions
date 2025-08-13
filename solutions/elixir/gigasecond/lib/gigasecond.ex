defmodule Gigasecond do
  @moduledoc false
  @type erlang_date_time ::
          {{pos_integer, pos_integer, pos_integer}, {pos_integer, pos_integer, pos_integer}}

  @doc """
  Calculate a date one billion seconds after an input date.
  """
  @spec from(erlang_date_time) :: erlang_date_time
  def from({{year, month, day}, {hours, minutes, seconds}}) do
    NaiveDateTime.new!(year, month, day, hours, minutes, seconds)
    |> NaiveDateTime.add(1_000_000_000)
    |> NaiveDateTime.to_erl()
  end
end
