defmodule RPNCalculator do
  @doc """
  Warn the team
  """
  def calculate!(stack, operation), do: operation.(stack)

  @doc """
  Wrap the error
  """
  def calculate(stack, operation) do
    try do
      {:ok, calculate!(stack, operation)}
    rescue
      _ -> :error
    end
  end

  @doc """
  Pass on the message
  """
  def calculate_verbose(stack, operation) do
    try do
      {:ok, calculate!(stack, operation)}
    rescue
      e in _ -> {:error, e.message}
    end
  end
end
