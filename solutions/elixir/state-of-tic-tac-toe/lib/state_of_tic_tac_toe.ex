defmodule StateOfTicTacToe do
  @type move :: {char, integer, integer}
  @type board_state :: %{char => [integer]}

  @initial_state %{?X => [], ?O => [], ?. => []}
  @magic_square [[2, 7, 6], [9, 5, 1], [4, 3, 8]]

  @extra_X_msg ~s(Wrong turn order: X went twice)
  @extra_O_msg ~s(Wrong turn order: O started)
  @both_win_msg ~s(Impossible board: game should have ended after the game was won)

  @doc """
  Determine the state a game of tic-tac-toe where X starts.

  The given board string will be converted into a map of magic square values.
    For example:
      XOO\nX..\nX..
    becomes
      %{46 => [5, 1, 3, 8], 79 => [7, 6], 88 => [2, 9, 4]}
  """
  @spec game_state(board :: String.t()) ::
          {:ok, :win | :ongoing | :draw} | {:error, String.t()}
  def game_state(board) do
    state = @initial_state |> Map.merge(convert(board))

    cond do
      extra_X?(state) -> {:error, @extra_X_msg}
      extra_O?(state) -> {:error, @extra_O_msg}
      both_win?(state) -> {:error, @both_win_msg}
      winner?(state[?X]) -> {:ok, :win}
      winner?(state[?O]) -> {:ok, :win}
      Enum.empty?(state[?.]) -> {:ok, :draw}
      true -> {:ok, :ongoing}
    end
  end

  # Determine the "magic square" values for each player's moves
  @spec convert(String.t()) :: board_state()
  defp convert(board) do
    String.split(board, "\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(&convert_row/1)
    |> Enum.group_by(&elem(&1, 0), &move_value(&1))
  end

  @spec convert_row({String.t(), integer}) :: [move]
  defp convert_row({row, n}) do
    String.to_charlist(row) |> Enum.with_index(&{&1, n, &2})
  end

  @spec move_value(move) :: integer
  defp move_value({_, row, col}) do
    @magic_square |> Enum.at(row) |> Enum.at(col)
  end

  # Check if any combination of three moves adds up to 15
  @spec winner?([integer]) :: boolean
  defp winner?(move_values) do
    combination(3, move_values) |> Enum.any?(&(Enum.sum(&1) == 15))
  end

  @spec both_win?(board_state) :: boolean
  defp both_win?(state), do: winner?(state[?X]) && winner?(state[?O])

  @spec extra_X?(board_state) :: boolean
  defp extra_X?(state), do: length(state[?X]) - length(state[?O]) > 1

  @spec extra_O?(board_state) :: boolean
  defp extra_O?(state), do: length(state[?X]) < length(state[?O])

  # Generate all possible sublists with n elements from given list
  @spec combination(integer, list) :: [list]
  defp combination(0, _), do: [[]]
  defp combination(_, []), do: []

  defp combination(n, [head | rest]) do
    for list <- combination(n - 1, rest) do
      [head | list]
    end ++ combination(n, rest)
  end
end
