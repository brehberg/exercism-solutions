defmodule Garden do
  import String, only: [codepoints: 1, split: 2]

  @students ~w(
    alice bob charlie david
    eve fred ginny harriet
    ileana joseph kincaid larry
  )a

  @plants %{
    "G" => :grass,
    "C" => :clover,
    "R" => :radishes,
    "V" => :violets
  }

  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """
  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @students) do
    student_names
    |> empty_map()
    |> Map.merge(
      split(info_string, "\n")
      |> Enum.map(&parse_row/1)
      |> group_by(student_names)
      |> Map.new()
    )
  end

  @spec empty_map(list) :: map
  defp empty_map(names), do: names |> Enum.map(&{&1, {}}) |> Enum.into(%{})

  @spec parse_row(String.t()) :: list
  defp parse_row(row_string), do: row_string |> codepoints() |> Enum.chunk_every(2)

  @spec group_by([list], [atom]) :: [{atom, tuple}]
  defp group_by(garden_rows, student_names) do
    [Enum.sort(student_names) | garden_rows]
    |> Enum.zip()
    |> Enum.map(&decode_plants/1)
  end

  @spec decode_plants({atom, list, list}) :: {atom, tuple}
  defp decode_plants({name, row1, row2}) do
    {name, (row1 ++ row2) |> Enum.map(&@plants[&1]) |> List.to_tuple()}
  end
end
