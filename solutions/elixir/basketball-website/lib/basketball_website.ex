defmodule BasketballWebsite do
  @moduledoc false

  @doc "Extract data from a nested map structure"
  @spec extract_from_path(%{}, String.t()) :: any
  def extract_from_path(data, path),
    do: do_extract(data, String.split(path, "."))

  @doc false
  @spec do_extract(%{}, [String.t()]) :: any
  defp do_extract(nil, _), do: nil
  defp do_extract(value, []), do: value

  defp do_extract(value, [current | pathlist]),
    do: do_extract(value[current], pathlist)

  @doc "Refactor using included functions"
  @spec get_in_path(%{}, String.t()) :: any
  def get_in_path(data, path),
    do: get_in(data, String.split(path, "."))
end
