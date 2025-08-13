defmodule TakeANumber do
  @moduledoc """
  An embedded system for a Take-A-Number machine. It is a very simple model.
  It can give out consecutive numbers and report what was the last number given out.
  """

  @doc """
  Start the machine

  Spawn a new process that has an initial state of 0 and is ready to receive messages.
  Returns the process's PID.
  """
  @spec start() :: pid
  def start(), do: spawn(fn -> machine_loop(0) end)

  # The machine's process needs to respond to messages. Uses recursion to wait for more
  # messages and passes the machine's state as an argument to the recursive function.
  @spec machine_loop(integer) :: nil
  defp machine_loop(state) do
    receive do
      {:report_state, sender_pid} -> respond(sender_pid, state)
      {:take_a_number, sender_pid} -> respond(sender_pid, state + 1)
      :stop -> nil
      _ -> machine_loop(state)
    end
  end

  # Send current state to sender_pid and then wait for more messages.
  @spec respond(pid, integer) :: nil
  defp respond(sender, state) do
    send(sender, state)
    machine_loop(state)
  end
end
