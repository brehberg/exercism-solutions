defmodule BoutiqueSuggestions do
  @default_opts [maximum_price: 100.00]

  @type item :: %{
          item_name: String.t(),
          price: Float,
          base_color: String.t()
        }

  @doc """
  Suggest a combination with filter out clashing outfits and by combination price
  """
  @spec get_combinations([item], [item], keyword()) :: [{item, item}]
  def get_combinations(tops, bottoms, options \\ []) do
    options = Keyword.merge(@default_opts, options)

    for top <- tops,
        bottom <- bottoms,
        top.base_color != bottom.base_color and
          top.price + bottom.price <= options[:maximum_price] do
      {top, bottom}
    end
  end
end
