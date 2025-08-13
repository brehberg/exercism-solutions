defmodule Frequency do
  import String

  @doc """
  Count letter frequency in parallel.

  Returns a map of characters to frequencies.

  The number of worker processes to use can be set with 'workers'.
  """
  @spec frequency([String.t()], pos_integer) :: map
  def frequency(texts, workers) do
    texts
    |> Task.async_stream(&letter_frequency/1, max_concurrency: workers, ordered: false)
    |> Enum.reduce(%{}, fn {:ok, frequency}, result ->
      Map.merge(result, frequency, fn _, v1, v2 -> v1 + v2 end)
    end)
  end

  defp letter_frequency(text) do
    text |> replace(~r/\P{L}/u, "") |> downcase() |> graphemes() |> Enum.frequencies()
  end
end
