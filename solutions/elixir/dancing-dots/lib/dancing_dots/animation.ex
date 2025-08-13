defmodule DancingDots.Animation do
  @moduledoc "Define the animation behaviour"
  @type dot :: DancingDots.Dot.t()
  @type opts :: keyword
  @type error :: any
  @type frame_number :: pos_integer

  @callback init(opts) :: {:ok, opts} | {:error, error}
  @callback handle_frame(dot, frame_number, opts) :: dot

  defmacro __using__(_) do
    quote do
      @behaviour DancingDots.Animation

      @doc "Provide a default implementation of the init/1 callback"
      @impl DancingDots.Animation
      def init(opts), do: {:ok, opts}
      defoverridable init: 1
    end
  end
end

defmodule DancingDots.Flicker do
  @moduledoc "Implement the Flicker animation"
  use DancingDots.Animation

  @doc """
  If the frame number is a multiple of four, the function should
  return the dot with half of its original opacity. In other
  frames, it should return the dot unchanged.
  """
  @impl DancingDots.Animation
  def handle_frame(dot, frame, _),
    do: if rem(frame, 4) == 0, do: %{dot | opacity: dot.opacity / 2}, else: dot
end

defmodule DancingDots.Zoom do
  @moduledoc "Implement the Zoom animation"
  use DancingDots.Animation

  @doc """
  This animation takes one option - velocity. Velocity can be any number.
  If it's negative, the dot gets zoomed out instead of zoomed in.
  """
  @impl DancingDots.Animation
  def init([velocity: value] = opts) when is_number(value), do: {:ok, opts}
  @impl DancingDots.Animation
  def init(opts), do: {:error, opts[:velocity] |> inspect() |> error_message()}

  @doc """
  Should return the dot with its radius increased by the
  current frame number, minus one, times velocity.
  """
  @impl DancingDots.Animation
  def handle_frame(dot, frame, velocity: value),
    do: %{dot | radius: dot.radius + (frame - 1) * value}

  @doc false
  @spec error_message(any) :: String.t()
  defp error_message(value),
    do: "The :velocity option is required, and its value must be a number. Got: #{value}"
end
