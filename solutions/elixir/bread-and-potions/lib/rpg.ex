defmodule RPG do
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
    def eat(term, term)
  end

  # Make loaves of bread edible
  defimpl Edible, for: LoafOfBread do
    def eat(%RPG.LoafOfBread{}, %RPG.Character{health: hp} = character) do
      {nil, %RPG.Character{character | health: hp + 5}}
    end
  end

  # Make mana potions edible
  defimpl Edible, for: ManaPotion do
    def eat(%RPG.ManaPotion{strength: str}, %RPG.Character{mana: mp} = character) do
      {%RPG.EmptyBottle{}, %RPG.Character{character | mana: mp + str}}
    end
  end

  # Make poisons edible
  defimpl Edible, for: Poison do
    def eat(%RPG.Poison{}, %RPG.Character{} = character) do
      {%RPG.EmptyBottle{}, %RPG.Character{character | health: 0}}
    end
  end
end
