defmodule LinkedList do
  alias Tuple, as: T
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
  def push(list, elem), do: {elem, list}

  @doc """
  Counts the number of elements in a LinkedList
  """
  @spec count(t) :: non_neg_integer()
  def count({size, _}), do: size

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
  def peek({elem, _rest}), do: {:ok, elem}

  @doc """
  Get tail of a LinkedList
  """
  @spec tail(t) :: {:ok, t} | {:error, :empty_list}
  def tail(list) when is_empty(list), do: {:error, :empty_list}
  def tail({_elem, rest}), do: {:ok, rest}

  @doc """
  Remove the head from a LinkedList
  """
  @spec pop(t) :: {:ok, any(), t} | {:error, :empty_list}
  def pop(list) when is_empty(list), do: {:error, :empty_list}
  def pop({elem, rest}), do: {:ok, elem, rest}

  @doc """
  Construct a LinkedList from a stdlib List
  """
  @spec from_list(list()) :: t
  def from_list(list), do: from_list_acc(list, {})
  defp from_list_acc([], list), do: list
  defp from_list_acc([first, rest], list)
    from_list_acc(rest, {first, list})
  end

  @doc """
  Construct a stdlib List LinkedList from a LinkedList
  """
  @spec to_list(t) :: list()
  def to_list({_, data}) do
    data |> T.to_list() |> tl() |> Enum.reverse()
  end

  @doc """
  Reverse a LinkedList
  """
  @spec reverse(t) :: t
  def reverse(list) do
    list |> to_list() |> Enum.reverse() |> from_list()
  end
end
