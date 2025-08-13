defmodule Garden do
  import String, only: [codepoints: 1, split: 2]

  @type plants :: :grass | :clover | :radishes | :violets
  @students ~w(alice bob charlie david
                eve fred ginny harriet
                ileana joseph kincaid larry)a

  @doc """
    Accepts a string representing the arrangement of cups on a windowsill and a
    list with names of students in the class. The student names list does not
    have to be in alphabetical order.

    It decodes that string into the various gardens for each student and returns
    that information in a map.
  """

  @spec info(String.t(), list) :: map
  def info(info_string, student_names \\ @students) do
    info_string
    |> split("\n")
    |> Enum.map(&parse_row/1)
    |> group_students(student_names)
    |> Enum.reduce(make_final(student_names), &map_plants/2)
  end

  @spec parse_row(String.t()) :: list
  defp parse_row(row_string), do: row_string |> codepoints() |> Enum.chunk_every(2)

  @spec group_students([list], list) :: [list]
  defp group_students(garden_rows, student_names) do
    [Enum.sort(student_names) | garden_rows] |> Enum.zip() |> Enum.map(&adjust_format/1)
  end

  @spec adjust_format({atom, list, list}) :: {atom, list}
  defp adjust_format({name, row1, row2}), do: {name, row1 ++ row2}

  @spec make_final(list) :: map
  defp make_final(names), do: names |> Enum.map(& {&1, {}}) |> Enum.into(%{})

  @spec map_plants({atom, list}, map) :: map
  defp map_plants({student, plants}, final) do
    Map.put(final, student, plants |> Enum.map(&plant_type/1) |> List.to_tuple())
  end

  @spec plant_type(String.t()) :: plants
  defp plant_type("G"), do: :grass
  defp plant_type("C"), do: :clover
  defp plant_type("R"), do: :radishes
  defp plant_type("V"), do: :violets
end
