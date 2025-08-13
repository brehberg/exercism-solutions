defmodule RPG do
  @moduledoc false

  defmodule Character do
    defstruct health: 100, mana: 0
  end

  defmodule LoafOfBread do
    defstruct []
  end

  defmodule ManaPotion do
    defstruct strength: 10
  end

  defmodule Poison do
    defstruct []
  end

  defmodule EmptyBottle do
    defstruct []
  end

  # Define edibility
  defprotocol Edible do
    @doc """
    The eat function accepts an item and a character and
    returns a by-product and a character.
    """
    @spec eat(struct, %RPG.Character{}) :: {struct | nil, %RPG.Character{}}
    def eat(item, character)
  end

  # Make loaves of bread edible
  defimpl Edible, for: LoafOfBread do
    @doc """
    When eaten, a loaf of bread gives the character 5 health points and
    has no by-product.
    """
    def eat(%RPG.LoafOfBread{}, %RPG.Character{health: hp} = character) do
      {nil, %RPG.Character{character | health: hp + 5}}
    end
  end

  # Make mana potions edible
  defimpl Edible, for: ManaPotion do
    @doc """
    When eaten, a mana potion gives the character as many mana points as
    the potion's strength, and produces an empty bottle.
    """
    def eat(%RPG.ManaPotion{strength: str}, %RPG.Character{mana: mp} = character) do
      {%RPG.EmptyBottle{}, %RPG.Character{character | mana: mp + str}}
    end
  end

  # Make poisons edible
  defimpl Edible, for: Poison do
    @doc """
    When eaten, a poison takes away all the health points from the
    character, and produces an empty bottle.
    """
    def eat(%RPG.Poison{}, %RPG.Character{} = character) do
      {%RPG.EmptyBottle{}, %RPG.Character{character | health: 0}}
    end
  end
end
