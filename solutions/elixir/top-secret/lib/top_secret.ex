defmodule TopSecret do
  @doc """
  Turn code into data
  """
  @spec to_ast(String.t()) :: Macro.t()
  def to_ast(string), do: Code.string_to_quoted!(string)

  @doc """
  Decode the secret message part from function definition
  """
  @spec decode_secret_message_part(Macro.t(), []) :: {Macro.t(), []}
  def decode_secret_message_part({op, _, [node | _]} = ast, acc)
      when op in [:def, :defp],
      do: {ast, [decode_function_ast(node) | acc]}

  def decode_secret_message_part(ast, acc), do: {ast, acc}

  @doc false
  @spec decode_function_ast({atom, keyword(), []}) :: String.t()
  defp decode_function_ast({:when, _, [node | _]}), do: decode_function_ast(node)
  defp decode_function_ast({_op, _, nil}), do: ""
  defp decode_function_ast({op, _, args}), do: format_function(Atom.to_string(op), args)
  @doc false
  @spec format_function(String.t(), []) :: String.t()
  defp format_function(name, args), do: name |> String.slice(0, length(args))

  @doc """
  Decode the full secret message
  """
  @spec decode_secret_message(String.t()) :: String.t()
  def decode_secret_message(string) do
    string
    |> to_ast()
    |> Macro.prewalk([], &decode_secret_message_part/2)
    |> elem(1)
    |> Enum.reverse()
    |> to_string()
  end
end
