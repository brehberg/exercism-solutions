defmodule Plot do
  @moduledoc """
  Plot struct as it is provided
  """
  @enforce_keys [:plot_id, :registered_to]
  defstruct [:plot_id, :registered_to]
end

defmodule CommunityGarden do
  @doc """
  Open the garden
  """
  @spec start(GenServer.options()) :: Agent.on_start()
  def start(opts \\ []) do
    Agent.start(fn -> %{plots: [], next: 1} end, opts)
  end

  @doc """
  List the registrations
  """
  @spec list_registrations(pid) :: [%Plot{}]
  def list_registrations(pid), do: Agent.get(pid, & &1.plots)

  @doc """
  Register plots to a person
  """
  @spec register(pid, String.t()) :: %Plot{}
  def register(pid, register_to) do
    Agent.update(pid, &register_next_plot(&1, register_to))
    Agent.get(pid, &hd(&1.plots))
  end

  @doc false
  @spec register_next_plot(Agent.state(), String.t()) :: Agent.state()
  defp register_next_plot(%{plots: plots, next: next}, name) do
    %{plots: [%Plot{plot_id: next, registered_to: name} | plots], next: next + 1}
  end

  @doc """
  Release plots
  """
  @spec release(pid, integer) :: :ok
  def release(pid, plot_id) do
    Agent.update(pid, &release_by_id(&1, plot_id))
  end

  @doc false
  @spec release_by_id(Agent.state(), integer) :: Agent.state()
  defp release_by_id(%{plots: plots} = state, id) do
    %{state | plots: Enum.reject(plots, fn plot -> plot.plot_id == id end)}
  end

  @doc """
  Get a registered plot
  """
  @spec get_registration(pid, integer) :: %Plot{} | {:not_found, String.t()}
  def get_registration(pid, plot_id) do
    case Agent.get(pid, &return_by_id(&1.plots, plot_id)) do
      nil -> {:not_found, "plot is unregistered"}
      found -> found
    end
  end

  @doc false
  @spec return_by_id([%Plot{}], integer) :: %Plot{}
  defp return_by_id(plots, id) do
    Enum.find(plots, fn plot -> plot.plot_id == id end)
  end
end
