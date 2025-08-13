defmodule BinarySearchTree do
  @moduledoc false
  import Enum, only: [concat: 2]
  @type bst_node :: %{data: any, left: bst_node | nil, right: bst_node | nil}

  @doc """
  Create a new Binary Search Tree with root's value as the given 'data'
  """
  @spec new(any) :: bst_node
  def new(data), do: %{data: data, left: nil, right: nil}

  @doc """
  Creates and inserts a node with its value as 'data' into the tree.
  """
  @spec insert(bst_node, any) :: bst_node
  def insert(tree, data) when is_nil(tree), do: new(data)
  def insert(tree, data) when data <= tree.data, do: %{tree | left: insert(tree.left, data)}
  def insert(tree, data) when data > tree.data, do: %{tree | right: insert(tree.right, data)}

  @doc """
  Traverses the Binary Search Tree in order and returns a list of each node's data.
  """
  @spec in_order(bst_node) :: [any]
  def in_order(tree) when is_nil(tree), do: []
  def in_order(tree), do: concat(in_order(tree.left), [tree.data | in_order(tree.right)])
end
