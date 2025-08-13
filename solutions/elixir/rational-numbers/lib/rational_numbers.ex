defmodule RationalNumbers do
  alias Kernel, as: K
  import Integer, only: [pow: 2, gcd: 2]

  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(rational, rational) :: rational
  def add({x1, y1}, {x2, y2}), do: {x1 * y2 + x2 * y1, y1 * y2} |> reduce()

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(rational, rational) :: rational
  def subtract({x1, y1}, {x2, y2}), do: {x1 * y2 - x2 * y1, y1 * y2} |> reduce()

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(rational, rational) :: rational
  def multiply({x1, y1}, {x2, y2}), do: {x1 * x2, y1 * y2} |> reduce()

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(rational, rational) :: rational
  def divide_by({x1, y1}, {x2, y2}), do: {x1 * y2, x2 * y1} |> reduce()

  @doc """
  Absolute value of a rational number
  """
  @spec abs(rational) :: rational
  def abs({x, y}), do: {K.abs(x), K.abs(y)} |> reduce()

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(rational, integer) :: rational
  def pow_rational({x, y}, n) when n >= 0, do: {pow(x, n), pow(y, n)} |> reduce()
  def pow_rational({x, y}, n), do: {pow(y, K.abs(n)), pow(x, K.abs(n))} |> reduce()

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(integer, rational) :: float
  def pow_real(n, {x, y}), do: Float.pow(n / 1, x / y)

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(rational) :: rational
  def reduce({x, y}) when y >= 0, do: {div(x, gcd(x, y)), div(y, gcd(x, y))}
  def reduce({x, y}), do: {-div(x, gcd(x, y)), -div(y, gcd(x, y))}
end
