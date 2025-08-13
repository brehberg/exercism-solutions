defmodule Chessboard do
  @doc """
  Define the rank range, from 1 to 8.
  """
  @spec rank_range :: Range.t(1, 8)
  def rank_range, do: 1..8

  @doc """
  Define the file range, from A to H
  """
  @spec file_range :: Range.t(?A, ?H)
  def file_range, do: ?A..?H

  @doc """
  Transform the rank range into a list of ranks
  """
  @spec ranks :: list(integer)
  def ranks, do: rank_range() |> Enum.to_list()

  @doc """
  Transform the file range into a list of files
  """
  @spec files :: list(String.t())
  def files, do: file_range() |> Enum.map(&<<&1>>)
end
