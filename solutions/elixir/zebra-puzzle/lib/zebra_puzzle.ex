defmodule ZebraPuzzle do
  defmodule House do
    defstruct color: nil, hobby: nil, drink: nil, nationality: nil, pet: nil
  end

  # Complete list of values:
  #
  #   Colors | Hobby     | Drink        | Nationality | Pet
  #   ---------------------------------------------------------
  #   Red    | Chess     | Coffee       | English     | Dog
  #   Green  | Football  | Tea          | Spaniard    | Fox
  #   Ivory  | Dancing   | Milk         | Ukranian    | Horse
  #   Yellow | Painting  | Orange Juice | Norwegian   | Zebra
  #   Blue   | Reading   | Water        | Japanese    | Snails

  @doc """
  Determine who drinks the water
  """
  @spec drinks_water() :: atom
  def drinks_water() do
    solve()
    |> Enum.find(fn c -> c.drink == :water end)
    |> Map.get(:nationality)
  end

  @doc """
  Determine who owns the zebra
  """
  @spec owns_zebra() :: atom
  def owns_zebra() do
    solve()
    |> Enum.find(fn c -> c.pet == :zebra end)
    |> Map.get(:nationality)
  end

  defp solve() do
    # Runtime of the solution solver will depend on the values of the seed.
    # The closer it is to correct, the less time it will take to complete.
    # Try swapping the positions of the fox and dog to see the difference.
    seed = [
      %House{
        color: :red,
        hobby: :painting,
        drink: :coffee,
        nationality: :english,
        pet: :dog
      },
      %House{
        color: :blue,
        hobby: :reading,
        drink: :tea,
        nationality: :norwegian,
        pet: :fox
      },
      %House{
        color: :green,
        hobby: :dancing,
        drink: :milk,
        nationality: :spaniard,
        pet: :horse
      },
      %House{
        color: :ivory,
        hobby: :football,
        drink: :orange_juice,
        nationality: :ukranian,
        pet: :zebra
      },
      %House{
        color: :yellow,
        hobby: :chess,
        drink: :water,
        nationality: :japanese,
        pet: :snails
      }
    ]

    do_solve([seed], MapSet.new())
  end

  defp do_solve([], _seen), do: :error

  defp do_solve([comb | remaining], seen) do
    if MapSet.member?(seen, comb) do
      do_solve(remaining, seen)
    else
      res =
        [
          &rule2/2,
          &rule3/2,
          &rule4/2,
          &rule5/2,
          &rule6/2,
          &rule7/2,
          &rule8/2,
          &rule9/2,
          &rule10/2,
          &rule11/2,
          &rule12/2,
          &rule13/2,
          &rule14/2,
          &rule15/2
        ]
        |> Enum.reduce({:ok, remaining}, fn elem, acc -> elem.(comb, acc) end)

      case res do
        {:ok, _queue} -> comb
        {:cont, queue} -> do_solve(queue, MapSet.put(seen, comb))
      end
    end
  end

  # The Englishman lives in the red house.
  defp rule2(comb, acc), do: house_rule(comb, [color: :red, nationality: :english], acc)

  # The Spaniard owns the dog.
  defp rule3(comb, acc), do: house_rule(comb, [nationality: :spaniard, pet: :dog], acc)

  # The person in the green house drinks coffee.
  defp rule4(comb, acc), do: house_rule(comb, [drink: :coffee, color: :green], acc)

  # The Ukranian drinks tea.
  defp rule5(comb, acc), do: house_rule(comb, [nationality: :ukranian, drink: :tea], acc)

  # The green house is immediately to the right of the ivory house.
  defp rule6(comb, acc) do
    ivory = Enum.find_index(comb, fn c -> c.color == :ivory end)
    green = Enum.find_index(comb, fn c -> c.color == :green end)

    house_on_right(comb, ivory, green, :color, acc)
  end

  # The snail owner likes to go dancing.
  defp rule7(comb, acc), do: house_rule(comb, [hobby: :dancing, pet: :snails], acc)

  # The person in the yellow house is a painter.
  defp rule8(comb, acc), do: house_rule(comb, [hobby: :painting, color: :yellow], acc)

  # The person in the middle house drinks milk.
  defp rule9(comb, acc), do: house_at_index(comb, 2, :drink, :milk, acc)

  # The Norwegian lives in the first house.
  defp rule10(comb, acc), do: house_at_index(comb, 0, :nationality, :norwegian, acc)

  # The person who enjoys reading lives in the house next to the person with the fox.
  defp rule11(comb, acc), do: house_adjacent(comb, [hobby: :reading, pet: :fox], acc)

  # The painter's house is next to the house with the horse.
  defp rule12(comb, acc), do: house_adjacent(comb, [hobby: :painting, pet: :horse], acc)

  # The person who plays football drinks orange juice.
  defp rule13(comb, acc), do: house_rule(comb, [hobby: :football, drink: :orange_juice], acc)

  # The Japanese person plays chess.
  defp rule14(comb, acc), do: house_rule(comb, [nationality: :japanese, hobby: :chess], acc)

  # The Norwegian lives next to the blue house.
  defp rule15(comb, acc), do: house_adjacent(comb, [nationality: :norwegian, color: :blue], acc)

  # For rules about one house being next to another.
  defp house_adjacent(comb, [{attr1, val1}, {attr2, val2}], acc) do
    entry1 = Enum.find_index(comb, fn c -> Map.get(c, attr1) == val1 end)
    entry2 = Enum.find_index(comb, fn c -> Map.get(c, attr2) == val2 end)
    do_house_adjacent(comb, entry1, entry2, attr1, attr2, acc)
  end

  defp do_house_adjacent(_comb, index1, index2, _attr1, _attr2, acc)
       when abs(index2 - index1) == 1,
       do: acc

  defp do_house_adjacent(comb, index1, index2, attr1, attr2, {_res, queue}) do
    q1 = arrange_left(comb, index1, index2, attr2, queue)
    q2 = arrange_right(comb, index1, index2, attr2, q1)
    q3 = arrange_left(comb, index2, index1, attr1, q2)
    q4 = arrange_right(comb, index2, index1, attr1, q3)
    {:cont, q4}
  end

  # For rules about a specific house.
  defp house_at_index(comb, index, attr, val, acc = {_res, queue}) do
    case Enum.find_index(comb, fn c -> Map.get(c, attr) == val end) do
      ^index -> acc
      match_index -> {:cont, [swap(comb, index, match_index, attr) | queue]}
    end
  end

  # Swap an attribute from index2 to put it just to the right of index1
  defp arrange_right(comb, index1, _index2, _attr, queue) when index1 == length(comb) - 1,
    do: queue

  defp arrange_right(comb, index1, index2, attr, queue),
    do: [swap(comb, index1 + 1, index2, attr) | queue]

  # Swap an attribute from index2 to put it just to the left of index1
  defp arrange_left(_comb, index1, _index2, _attr, queue) when index1 == 0, do: queue

  defp arrange_left(comb, index1, index2, attr, queue),
    do: [swap(comb, index1 - 1, index2, attr) | queue]

  # For rule 6, check index2 is immediately to the right of index1
  defp house_on_right(_comb, index1, index2, _attr, acc) when index2 == index1 + 1, do: acc

  defp house_on_right(comb, index1, index2, attr, {_res, queue}) do
    q1 = arrange_right(comb, index1, index2, attr, queue)
    q2 = arrange_left(comb, index2, index1, attr, q1)
    {:cont, q2}
  end

  # For rules about one of the houses.
  defp house_rule(comb, [{attr1, val1}, {attr2, val2}], acc) do
    entry1 = Enum.find_index(comb, fn c -> Map.get(c, attr1) == val1 end)
    entry2 = Enum.find_index(comb, fn c -> Map.get(c, attr2) == val2 end)
    swap_attr(comb, entry1, entry2, attr1, attr2, acc)
  end

  # Swap an attribute from index1 and index2, if they are not the same, and add the combination to the queue.
  defp swap_attr(_comb, index1, index2, _attr1, _attr2, acc)
       when not is_nil(index1) and not is_nil(index2) and index1 == index2,
       do: acc

  defp swap_attr(comb, index1, index2, attr1, attr2, {_res, queue}) do
    alt1 = swap(comb, index1, index2, attr1)
    alt2 = swap(comb, index1, index2, attr2)

    {:cont, [alt1, alt2 | queue]}
  end

  # Swap the attributes at two indexes.
  defp swap(comb, index1, index2, attr) do
    # v2 = Enum.at(comb, index2)
    {attr2_val, next_comb} =
      get_and_update_in(comb, [Access.at(index2), Access.key!(attr)], fn v ->
        {v, get_in(comb, [Access.at(index1), Access.key!(attr)])}
      end)

    update_in(next_comb, [Access.at(index1), Access.key!(attr)], fn _v -> attr2_val end)
  end
end
