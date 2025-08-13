defmodule RPNCalculatorInspection do
  @moduledoc """
  Conduct two types of checks:
  1) reliability check that will detect inputs for which the calculator
     under inspection either crashes or doesn't respond fast enough.
  2) correctness check that will check if for a given input, the result
     returned by the calculator is as expected.
  """
  import Task, only: [async: 1, await: 2]

  @type reliability_check :: %{input: String.t(), pid: pid}
  @type check_result :: %{String.t() => :ok | :error | :timeout}
  @doc """
  Start a reliability check for a single input
  """
  @spec start_reliability_check((String.t() -> integer), String.t()) :: reliability_check
  def start_reliability_check(calculator, input) do
    %{input: input, pid: spawn_link(fn -> calculator.(input) end)}
  end

  @doc """
  Interpret the results of a reliability check
  """
  @spec await_reliability_check_result(reliability_check, check_result) :: check_result
  def await_reliability_check_result(%{pid: pid, input: input}, results) do
    receive do
      {:EXIT, ^pid, :normal} -> Map.put(results, input, :ok)
      {:EXIT, ^pid, _} -> Map.put(results, input, :error)
    after
      100 -> Map.put(results, input, :timeout)
    end
  end

  @doc """
  Run a concurrent reliability check for many inputs
  """
  @spec reliability_check((String.t() -> integer), [String.t()]) :: check_result
  def reliability_check(_calculator, []), do: %{}

  def reliability_check(calculator, inputs) do
    old_trap_exit_flag = Process.flag(:trap_exit, true)

    results =
      inputs
      |> Enum.map(&start_reliability_check(calculator, &1))
      |> Enum.reduce(%{}, &await_reliability_check_result(&1, &2))

    Process.flag(:trap_exit, old_trap_exit_flag)
    results
  end

  @doc """
  Run a concurrent correctness check for many inputs
  """
  @spec correctness_check((String.t() -> integer), [String.t()]) :: [integer]
  def correctness_check(_calculator, []), do: []

  def correctness_check(calculator, inputs) do
    inputs
    |> Enum.map(&async(fn -> calculator.(&1) end))
    |> Enum.map(&await(&1, 100))
  end
end
