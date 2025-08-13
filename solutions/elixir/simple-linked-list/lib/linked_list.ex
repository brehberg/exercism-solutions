defmodule LinkedList do
  @opaque t :: tuple()

  defguardp is_empty(list) when list == {}

  @doc """
  Construct a new LinkedList
  """
  @spec new() :: t
  def new(), do: {}

  @doc """
  Push an item onto a LinkedList
  """
  @spec push(t, any()) :: t
  def push(list, element), do: {element, list}

  @doc """
  Counts the number of elements in a LinkedList
  """
  @spec count(t) :: non_neg_integer()
  def count(list), do: count_acc(list, 0)
  defp count_acc(list, size) when is_empty(list), do: size
  defp count_acc({_, rest}, size), do: count_acc(rest, size + 1)

  @doc """
  Determine if a LinkedList is empty
  """
  @spec empty?(t) :: boolean()
  def empty?(list) when is_empty(list), do: true
  def empty?(_), do: false

  @doc """
  Get the value of a head of the LinkedList
  """
  @spec peek(t) :: {:ok, any()} | {:error, :empty_list}
  def peek(list) when is_empty(list), do: {:error, :empty_list}
  def peek({element, _}), do: {:ok, element}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail(list) when is_empty(list), do: {:error, :empty_list}
  def tail({_, rest}), do: {:ok, rest}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop(list) when is_empty(list), do: {:error, :empty_list}
  def pop({element, rest}), do: {:ok, element, rest}

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list([]), do: {}
  def from_list([first | rest]), do: {first, from_list(rest)}

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list({}), do: []
  def to_list({first, rest}), do: [first | to_list(rest)]

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list), do: reverse_acc(list, {})
  defp reverse_acc({}, list), do: list
  defp reverse_acc({first, rest}, list), do: reverse_acc(rest, {first, list})
end
