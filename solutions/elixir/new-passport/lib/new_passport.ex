defmodule NewPassport do
  @moduledoc false

  @doc """
  Do your best to always "focus on the happy path"

  1) Get into the building
  2) Go to the information desk and find which counter you should go to
  3) Go to the counter and get your form stamped
  4) Receive your new passport
  """
  @spec get_new_passport(%NaiveDateTime{}, %Date{}, atom) :: any
  def get_new_passport(now, birthday, form) do
    with {:ok, entry} <- enter_building(now),
         {:ok, func} <- find_counter_information(now),
         {:ok, checksum} <- stamp_form(entry, func.(birthday), form) do
      {:ok, get_new_passport_number(entry, func.(birthday), checksum)}
    else
      {:coffee_break, _} -> {:retry, NaiveDateTime.add(now, 15 * 60)}
      error -> error
    end
  end

  # Do not modify the functions below

  defp enter_building(%NaiveDateTime{} = datetime) do
    day = Date.day_of_week(datetime)
    time = NaiveDateTime.to_time(datetime)

    cond do
      day <= 4 and time_between(time, ~T[13:00:00], ~T[15:30:00]) ->
        {:ok, datetime |> DateTime.from_naive!("Etc/UTC") |> DateTime.to_unix()}

      day == 5 and time_between(time, ~T[13:00:00], ~T[14:30:00]) ->
        {:ok, datetime |> DateTime.from_naive!("Etc/UTC") |> DateTime.to_unix()}

      true ->
        {:error, "city office is closed"}
    end
  end

  @eighteen_years 18 * 365
  defp find_counter_information(%NaiveDateTime{} = datetime) do
    time = NaiveDateTime.to_time(datetime)

    if time_between(time, ~T[14:00:00], ~T[14:20:00]) do
      {:coffee_break, "information counter staff on coffee break, come back in 15 minutes"}
    else
      {:ok, fn %Date{} = birthday -> 1 + div(Date.diff(datetime, birthday), @eighteen_years) end}
    end
  end

  defp stamp_form(timestamp, counter, :blue) when rem(counter, 2) == 1 do
    {:ok, 3 * (timestamp + counter) + 1}
  end

  defp stamp_form(timestamp, counter, :red) when rem(counter, 2) == 0 do
    {:ok, div(timestamp + counter, 2)}
  end

  defp stamp_form(_timestamp, _counter, _form), do: {:error, "wrong form color"}

  defp get_new_passport_number(timestamp, counter, checksum) do
    "#{timestamp}-#{counter}-#{checksum}"
  end

  defp time_between(time, from, to) do
    Time.compare(from, time) != :gt and Time.compare(to, time) == :gt
  end
end
