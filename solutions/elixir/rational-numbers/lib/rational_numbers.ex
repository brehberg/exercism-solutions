defmodule RationalNumbers do
  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(a :: rational, b :: rational) :: rational
  def add({x1, y1}, {x2, y2}), do: reduce({x1 * y2 + x2 * y1, y1 * y2})

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(a :: rational, b :: rational) :: rational
  def subtract({x1, y1}, {x2, y2}), do: reduce({x1 * y2 - x2 * y1, y1 * y2})

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(a :: rational, b :: rational) :: rational
  def multiply({x1, y1}, {x2, y2}), do: reduce({x1 * x2, y1 * y2})

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(num :: rational, den :: rational) :: rational
  def divide_by({x1, y1}, {x2, y2}), do: reduce({x1 * y2, x2 * y1})

  @doc """
  Absolute value of a rational number
  """
  @spec abs(a :: rational) :: rational
  def abs({x1, y1}), do: reduce({Kernel.abs(x1), Kernel.abs(y1)})

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(a :: rational, n :: integer) :: rational
  def pow_rational({x1, y1}, n) when n >= 0, do: reduce({pow(x1, n), pow(y1, n)})
  def pow_rational({x1, y1}, n), do: reduce({pow(y1, Kernel.abs(n)), pow(x1, Kernel.abs(n))})
  @doc false
  defp pow(_, 0), do: 1
  defp pow(x, 1), do: x
  defp pow(x, n), do: x * pow(x, n-1)

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(x :: integer, n :: rational) :: float
  def pow_real(x, {x1, y1}) when x1 >= 0, do: root(pow(x, x1), y1)
  def pow_real(x, {x1, y1}), do: 1 / root(pow(x, Kernel.abs(x1)), y1)
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
  @spec reduce(a :: rational) :: rational
  def reduce({num, den}) when den >= 0, do:
    {div(num, Integer.gcd(num, den)), div(den, Integer.gcd(num, den))}
  def reduce({num, den}), do:
    {div(num, Integer.gcd(num, den)) * -1, div(den, Integer.gcd(num, den)) * -1}

end
