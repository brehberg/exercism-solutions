defmodule Raindrops do
  @moduledoc false
  @raindrops [{3, "Pling"}, {5, "Plang"}, {7, "Plong"}]

  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """
  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    sound = Enum.map_join(@raindrops, &sound(&1, number))
    if sound == "", do: to_string(number), else: sound
  end

  @doc false
  @spec(sound({integer, String.t()}, pos_integer) :: String.t())
  defp sound({factor, value}, n) when rem(n, factor) == 0, do: value
  defp sound(_, _), do: ""
end
