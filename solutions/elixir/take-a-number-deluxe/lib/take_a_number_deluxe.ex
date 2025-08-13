defmodule TakeANumberDeluxe do
  @moduledoc """
  Take-A-Number machine features added to the deluxe model include:

  1) Keeping track of currently queued numbers.
  2) Setting the minimum and maximum number.
  3) Allowing certain numbers to skip the queue to provide priority service.
  4) Auto shutdown to prevent accidentally leaving the machine on.
  """
  use GenServer

  # Client API
  @type machine_state :: TakeANumberDeluxe.State.t()

  @doc "Start the machine"
  @spec start_link(keyword()) :: {:ok, pid()} | {:error, atom()}
  def start_link(init_arg), do: GenServer.start_link(__MODULE__, init_arg)

  @doc "Report machine state"
  @spec report_state(pid()) :: machine_state()
  def report_state(machine), do: GenServer.call(machine, :report_state)

  @doc "Queue new numbers"
  @spec queue_new_number(pid()) :: {:ok, integer()} | {:error, atom()}
  def queue_new_number(machine), do: GenServer.call(machine, :queue_number)

  @doc "Serve next queued number"
  @spec serve_next_queued_number(pid(), integer() | nil) :: {:ok, integer()} | {:error, atom()}
  def serve_next_queued_number(machine, priority_number \\ nil),
    do: GenServer.call(machine, {:serve_next, priority_number})

  @doc "Reset state"
  @spec reset_state(pid()) :: :ok
  def reset_state(machine), do: GenServer.cast(machine, :reset_state)

  # Server callbacks

  @doc """
  Callback return value dictates if the server can be started successfully.

  :ok    The server will start its receive loop with initial state
  :stop  The process will exit with the given reason
  """
  @impl GenServer
  def init(init_arg) do
    min_number = Keyword.get(init_arg, :min_number)
    max_number = Keyword.get(init_arg, :max_number)
    auto_shutdown_timeout = Keyword.get(init_arg, :auto_shutdown_timeout, :infinity)

    case TakeANumberDeluxe.State.new(min_number, max_number, auto_shutdown_timeout) do
      {:ok, state} -> {:ok, state, state.auto_shutdown_timeout}
      {:error, reason} -> {:stop, reason}
    end
  end

  @doc """
  Callback responsible for handling and responding to synchronous messages.

  :report_state  Report machine state
  :queue_number  Queue new numbers
  :serve_next    Serve next queued number
  """
  @impl GenServer
  def handle_call(:report_state, _from, state), do: reply_with_timeout(state, state)

  @impl GenServer
  def handle_call(:queue_number, _from, state) do
    case TakeANumberDeluxe.State.queue_new_number(state) do
      {:ok, number, new_state} -> reply_with_timeout({:ok, number}, new_state)
      {:error, reason} -> reply_with_timeout({:error, reason}, state)
    end
  end

  @impl GenServer
  def handle_call({:serve_next, priority}, _from, state) do
    case TakeANumberDeluxe.State.serve_next_queued_number(state, priority) do
      {:ok, number, new_state} -> reply_with_timeout({:ok, number}, new_state)
      {:error, reason} -> reply_with_timeout({:error, reason}, state)
    end
  end

  @doc false
  @spec reply_with_timeout(term, machine_state) :: {:reply, term, machine_state, timeout}
  defp reply_with_timeout(reply, state), do: {:reply, reply, state, state.auto_shutdown_timeout}

  @doc """
  Callback responsible for handling messages that don't require a reply.

  :reset_state  Reset state
  """
  @impl GenServer
  def handle_cast(:reset_state, state) do
    case TakeANumberDeluxe.State.new(
           state.min_number,
           state.max_number,
           state.auto_shutdown_timeout
         ) do
      {:ok, new_state} -> noreply_with_timeout(new_state)
      {:error, reason} -> {:stop, reason}
    end
  end

  @doc """
  Callback responsible for handling messages in the server's inbox other than calling.

  :timeout  Implement auto shutdown
  """
  @impl GenServer
  def handle_info(message, state) do
    case message do
      :timeout -> {:stop, :normal, state}
      _ -> noreply_with_timeout(state)
    end
  end

  @doc false
  @spec noreply_with_timeout(machine_state) :: {:noreply, machine_state, timeout}
  defp noreply_with_timeout(state), do: {:noreply, state, state.auto_shutdown_timeout}
end
