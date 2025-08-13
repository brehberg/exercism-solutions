defmodule RPNCalculator.Exception do
  defmodule DivisionByZeroError do
    @moduledoc """
    Error for Division by Zero
    """
    defexception message: "division by zero occurred"
  end

  defmodule StackUnderflowError do
    @moduledoc """
    Error when encountering stack underflow
    """
    defexception message: "stack underflow occurred"
    @context_message "stack underflow occurred, context: "

    @impl true
    def exception(value) do
      case value do
        [] -> %StackUnderflowError{}
        _ -> %StackUnderflowError{message: @context_message <> value}
      end
    end
  end

  @doc """
  Write a dividing function
  """
  def divide([]), do: raise(StackUnderflowError, "when dividing")
  def divide([_]), do: raise(StackUnderflowError, "when dividing")
  def divide([0 | [_]]), do: raise(DivisionByZeroError)
  def divide([y | [x]]), do: x / y
end
