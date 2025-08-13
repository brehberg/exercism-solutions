defmodule PerfectNumbers do
  @moduledoc false

  @doc """
  Determine the aliquot sum of the given `number`, by summing all the factors
  of `number`, aside from `number` itself.

  Based on this sum, classify the number as:

  :perfect if the aliquot sum is equal to `number`
  :abundant if the aliquot sum is greater than `number`
  :deficient if the aliquot sum is less than `number`
  """
  @spec classify(number :: integer) :: {:ok, atom} | {:error, String.t()}
  def classify(number) when number < 1,
    do: {:error, "Classification is only possible for natural numbers."}

  def classify(number),
    do: number |> aliquot_sum({1, number}, []) |> classifier(number)

  @doc false
  @spec classifier(sum :: integer, number :: integer) :: {:ok, atom}
  defp classifier(sum, n) when sum == n, do: {:ok, :perfect}
  defp classifier(sum, n) when sum > n, do: {:ok, :abundant}
  defp classifier(sum, n) when sum < n, do: {:ok, :deficient}

  @doc false
  @spec aliquot_sum(integer, {integer, integer}, []) :: integer
  defp aliquot_sum(_, {next, last}, factors) when next >= last - 1,
    do: Enum.sum(factors)

  defp aliquot_sum(n, {next, _}, factors) when rem(n, next) == 0 do
    last = div(n, next)

    if last == next or last == n do
      aliquot_sum(n, {next + 1, last}, [next | factors])
    else
      aliquot_sum(n, {next + 1, last}, [next, last | factors])
    end
  end

  defp aliquot_sum(n, {next, last}, factors),
    do: aliquot_sum(n, {next + 1, last}, factors)
end
