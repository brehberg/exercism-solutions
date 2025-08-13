defmodule DndCharacter do
  @moduledoc false
  import Enum

  @type t :: %__MODULE__{
          strength: pos_integer(),
          dexterity: pos_integer(),
          constitution: pos_integer(),
          intelligence: pos_integer(),
          wisdom: pos_integer(),
          charisma: pos_integer(),
          hitpoints: pos_integer()
        }

  defstruct ~w[strength dexterity constitution intelligence wisdom charisma hitpoints]a
  @die6 1..6
  @base_hp 10

  @doc """
  Find your modifier by subtracting 10 from your score, divide by 2 and round down.
  """
  @spec modifier(pos_integer()) :: integer()
  def modifier(score), do: ((score - 10) / 2) |> floor()

  @doc """
  The ability scores are determined randomly by rolling four 6-sided dice
  and record the sum of the largest three dice.
  """
  @spec ability :: pos_integer()
  def ability,
    do: for(_ <- 1..4, do: random(@die6)) |> sort() |> drop(1) |> sum()

  @doc """
  This character has, among other things, six abilities;
    strength, dexterity, constitution, intelligence, wisdom, and charisma.
  The character's initial hitpoints are 10 + the character's constitution modifier.
  """
  @spec character :: t()
  def character do
    constitution = ability()

    %DndCharacter{
      strength: ability(),
      dexterity: ability(),
      constitution: constitution,
      intelligence: ability(),
      wisdom: ability(),
      charisma: ability(),
      hitpoints: @base_hp + modifier(constitution)
    }
  end
end
