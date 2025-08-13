defmodule Chessboard do
  @doc """
  Define the rank range, from 1 to 8.
  """
  @spec rank_range :: Range.t(integer)
  def rank_range, do: 1..8

  @doc """
  Define the file range, from A to H
  """
  @spec rank_range :: Range.t(char)
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
