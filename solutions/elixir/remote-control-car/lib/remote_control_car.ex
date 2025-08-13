defmodule RemoteControlCar do
  @moduledoc """
  Playing around with a remote controlled car
  """
  @enforce_keys [:nickname]
  defstruct [:nickname, battery_percentage: 100, distance_driven_in_meters: 0]

  @doc """
  Create a brand-new remote controlled car, with optional nickname
  """
  def new(), do: %RemoteControlCar{nickname: "none"}
  def new(nickname \\ "none"), do: %RemoteControlCar{nickname: nickname}

  @doc """
  Display the distance
  """
  def display_distance(%RemoteControlCar{} = rc), do: "#{rc.distance_driven_in_meters} meters"

  @doc """
  Display the battery percentage
  """
  def display_battery(%RemoteControlCar{battery_percentage: 0}), do: "Battery empty"
  def display_battery(%RemoteControlCar{} = rc), do: "Battery at #{rc.battery_percentage}%"

  @doc """
  Driving changes the battery and distance driven, unless dead battery
  """
  def drive(%RemoteControlCar{battery_percentage: 0} = rc), do: rc

  def drive(%RemoteControlCar{} = rc),
    do: %{
      rc
      | battery_percentage: rc.battery_percentage - 1,
        distance_driven_in_meters: rc.distance_driven_in_meters + 20
    }
end
