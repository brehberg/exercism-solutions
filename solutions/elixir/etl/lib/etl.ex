defmodule ETL do
  @moduledoc false
  import String, only: [downcase: 1]

  @type old_map :: %{integer => [String.t()]}
  @type new_map :: %{String.t() => integer}

  @doc """
  Transforms an old Scrabble score system to a new one.

  ## Examples

    iex> ETL.transform(%{1 => ["A", "E"], 2 => ["D", "G"]})
    %{"a" => 1, "d" => 2, "e" => 1, "g" => 2}
  """
  @spec transform(old_map) :: new_map
  def transform(input) do
    for {value, letters} <- input,
        letter <- letters,
        into: %{} do
      &{downcase(letter), value}
    end
  end
end
