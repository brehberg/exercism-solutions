class BoutiqueInventory
  def initialize(items)
    @items = items
  end

  # Return a list of the names of the items
  def item_names
    @items.map { |item| item[:name] }.sort
  end

  # Return any items that are cheap
  def cheap
    @items.select { |item| item[:price] < 30 }
  end

  # Return any items that are out of stock
  def out_of_stock
    @items.select { |item| item[:quantity_by_size].empty? }
  end

  # Return a single item's stock
  def stock_for_item(name)
    @items.find { |item| item[:name] == name }[:quantity_by_size]
  end

  # Return the total stock
  def total_stock
    @items.sum { |item|
      item[:quantity_by_size].sum { |_size, qty| qty }
    }
  end

  private

  attr_reader :items
end
