defmodule ListOps do
  @moduledoc """
  Implement basic list operations.
  """

  @doc "given a list, return the total number of items within it"
  @spec count(list) :: non_neg_integer
  def count(l), do: do_count(l, 0)
  defp do_count([], c), do: c
  defp do_count([_ | t], c), do: do_count(t, c + 1)

  @doc "given a list, return a list with all the original items, but in reversed order"
  @spec reverse(list) :: list
  def reverse(l), do: do_reverse(l, [])
  defp do_reverse([], r), do: r
  defp do_reverse([h | t], r), do: do_reverse(t, [h | r])

  @doc "given a function and a list, return the list of the results of applying function on all items"
  @spec map(list, (any -> any)) :: list
  def map(l, func), do: do_map(l, func, [])
  defp do_map([], _, m), do: reverse(m)
  defp do_map([h | t], f, m), do: do_map(t, f, [f.(h) | m])

  @doc "given a predicate and a list, return the list of all items for which predicate is True"
  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, func), do: do_filter(l, func, [])
  defp do_filter([], _, r), do: reverse(r)
  defp do_filter([h | t], f, r), do: do_filter(t, f, if(f.(h), do: [h | r], else: r))

  @doc "given a list, an initial accumulator, and a function
        reduce each item into the accumulator from the left"
  @type acc :: any
  @spec foldl(list, acc, (any, acc -> acc)) :: acc
  def foldl([], acc, _), do: acc
  def foldl([h | t], acc, func), do: foldl(t, func.(h, acc), func)

  @doc "given a list, an initial accumulator, and a function
        reduce each item into the accumulator from the right"
  @spec foldr(list, acc, (any, acc -> acc)) :: acc
  def foldr(l, acc, func), do: reverse(l) |> foldl(acc, func)

  @doc "given two lists, add all items in the second list to the end of the first list"
  @spec append(list, list) :: list
  def append(a, b), do: reverse(a) |> do_append(b)
  defp do_append([], b), do: b
  defp do_append([h | t], b), do: do_append(t, [h | b])

  @doc "given a series of lists, combine all items in all lists into one flattened list"
  @spec concat([[any]]) :: [any]
  def concat(ll), do: reverse(ll) |> do_concat([])
  defp do_concat([], r), do: r
  defp do_concat([h | t], r), do: do_concat(t, append(h, r))
end
