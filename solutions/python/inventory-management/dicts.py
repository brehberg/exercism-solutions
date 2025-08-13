"""Functions to keep track and alter inventory."""


def create_inventory(items):
    """Create a dict that tracks the amount (count) of each element on the `items` list.

    :param items: list - list of items to create an inventory from.
    :return: dict - the inventory dictionary.
    """

    def add_item_to_inventory(item, inv):
        inv[item] = inv.get(item, 0) + 1

    result = {}
    [add_item_to_inventory(item, result) for item in items]
    return result


def add_items(inventory, items):
    """Add or increment items in inventory using elements from the items `list`.

    :param inventory: dict - dictionary of existing inventory.
    :param items: list - list of items to update the inventory with.
    :return: dict - the inventory updated with the new items.
    """

    def add_item_to_inventory(item, inv):
        inv[item] = inv.get(item, 0) + 1

    result = inventory
    [add_item_to_inventory(item, result) for item in items]
    return result


def decrement_items(inventory, items):
    """Decrement items in inventory using elements from the `items` list.

    :param inventory: dict - inventory dictionary.
    :param items: list - list of items to decrement from the inventory.
    :return: dict - updated inventory with items decremented.
    """

    def reduce_item_in_inventory(item, inv):
        try:
            item_count = inv[item]
            inv[item] = item_count - 1 if item_count > 0 else 0
        except KeyError:
            pass

    result = inventory
    [reduce_item_in_inventory(item, result) for item in items]
    return result


def remove_item(inventory, item):
    """Remove item from inventory if it matches `item` string.

    :param inventory: dict - inventory dictionary.
    :param item: str - item to remove from the inventory.
    :return: dict - updated inventory with item removed. Current inventory if item does not match.
    """

    try:
        inventory.pop(item)
    except KeyError:
        pass
    return inventory


def list_inventory(inventory):
    """Create a list containing all (item_name, item_count) pairs in inventory.

    :param inventory: dict - an inventory dictionary.
    :return: list of tuples - list of key, value pairs from the inventory dictionary.
    """

    return [(item, count) for item, count in inventory.items() if count > 0]
