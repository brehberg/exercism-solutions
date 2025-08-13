defmodule RationalNumbers do
  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(rational, rational) :: rational
  def add({x1, y1}, {x2, y2}), do: reduce({x1 * y2 + x2 * y1, y1 * y2})

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(rational, rational) :: rational
  def subtract({x1, y1}, {x2, y2}), do: reduce({x1 * y2 - x2 * y1, y1 * y2})

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(rational, rational) :: rational
  def multiply({x1, y1}, {x2, y2}), do: reduce({x1 * x2, y1 * y2})

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(rational, rational) :: rational
  def divide_by({x1, y1}, {x2, y2}), do: reduce({x1 * y2, x2 * y1})

  @doc """
  Absolute value of a rational number
  """
  @spec abs(rational) :: rational
  def abs({x1, y1}), do: reduce({Kernel.abs(x1), Kernel.abs(y1)})

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(rational, integer) :: rational
  def pow_rational({x1, y1}, n) when n >= 0, do: reduce({Integer.pow(x1, n), Integer.pow(y1, n)})
  def pow_rational({x1, y1}, n), do: reduce({Integer.pow(y1, Kernel.abs(n)), Integer.pow(x1, Kernel.abs(n))})

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(integer, rational) :: float
  def pow_real(x, {x1, y1}) when x1 >= 0, do: root(Integer.pow(x, x1), y1)
  def pow_real(x, {x1, y1}), do: 1 / root(Integer.pow(x, Kernel.abs(x1)), y1)
  @doc false
  defp root(p, q), do: nth_root(q, p)
  defp nth_root(n, x, precision \\ 1.0e-5) do
    f = fn(prev) -> ((n-1) * prev + x / :math.pow(prev, (n-1))) / n end
    fixed_point(f, x, precision, f.(x))
  end
  defp fixed_point(_, guess, tolerance, next) when Kernel.abs(guess - next) < tolerance, do: next
  defp fixed_point(f, _, tolerance, next), do: fixed_point(f, next, tolerance, f.(next))

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(rational) :: rational
  def reduce({x1, y1}) when y1 >= 0, do: {div(x1, Integer.gcd(x1, y1)), div(y1, Integer.gcd(x1, y1))}
  def reduce({x1, y1}), do: {div(x1, Integer.gcd(x1, y1)) * -1, div(y1, Integer.gcd(x1, y1)) * -1}

end
