defmodule NameBadge do
  @doc """
    Return the badge label, with the department name in uppercase.
  """
  @spec print(id :: integer, name :: String.t(), department :: String.t()) :: String.t()
  def print(id, name, department), do: "#{prefix(id)}#{name} - #{format(department)}"

  @doc false
  @spec format(String.t()) :: String.t()
  defp format(department), do: if(department, do: String.upcase(department), else: "OWNER")
  @spec prefix(integer) :: String.t()
  defp prefix(id), do: if(id, do: "[#{id}] - ", else: "")
end
