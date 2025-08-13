defmodule TakeANumberDeluxe do
  alias TakeANumberDeluxe.State, as: TANDState
  use GenServer

  # Client API

  @doc "Start the machine"
  @spec start_link(keyword()) :: {:ok, pid()} | {:error, atom()}
  def start_link(init_arg), do: GenServer.start_link(__MODULE__, init_arg)

  @doc "Report machine state"
  @spec report_state(pid()) :: TakeANumberDeluxe.State.t()
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

  @doc "Start the machine"
  @impl GenServer
  def init(init_arg) do
    init_arg = Keyword.merge([auto_shutdown_timeout: :infinity], init_arg)

    case TANDState.new(
           init_arg[:min_number],
           init_arg[:max_number],
           init_arg[:auto_shutdown_timeout]
         ) do
      {:ok, state} -> {:ok, state, state.auto_shutdown_timeout}
      {:error, reason} -> {:stop, reason}
    end
  end

  @doc """
  Report machine state
  Queue new numbers
  Serve next queued number
  """
  @impl GenServer
  def handle_call(:report_state, _from, state) do
    {:reply, state, state, state.auto_shutdown_timeout}
  end

  @impl GenServer
  def handle_call(:queue_number, _from, state) do
    case TANDState.queue_new_number(state) do
      {:ok, number, state} -> {:reply, {:ok, number}, state, state.auto_shutdown_timeout}
      error -> {:reply, error, state, state.auto_shutdown_timeout}
    end
  end

  @impl GenServer
  def handle_call({:serve_next, priority}, _from, state) do
    case TANDState.serve_next_queued_number(state, priority) do
      {:ok, number, state} -> {:reply, {:ok, number}, state, state.auto_shutdown_timeout}
      error -> {:reply, error, state, state.auto_shutdown_timeout}
    end
  end

  @doc "Reset state"
  @impl GenServer
  def handle_cast(:reset_state, state) do
    case TANDState.new(
           state.min_number,
           state.max_number,
           state.auto_shutdown_timeout
         ) do
      {:ok, state} -> {:noreply, state, state.auto_shutdown_timeout}
      {:error, reason} -> {:stop, reason}
    end
  end

  @doc "Implement auto shutdown"
  @impl GenServer
  def handle_info(message, state) do
    case message do
      :timeout -> {:stop, :normal, state}
      _ -> {:noreply, state, state.auto_shutdown_timeout}
    end
  end
end
