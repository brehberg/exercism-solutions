defmodule RationalNumbers do
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
  def abs({x, y}), do: {Kernel.abs(x), Kernel.abs(y)} |> reduce()

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(rational, integer) :: rational
  def pow_rational({x, y}, n) do
    case n >= 0 do
      true -> {Integer.pow(x, n), Integer.pow(y, n)} |> reduce()
      false -> {Integer.pow(y, Kernel.abs(n)), Integer.pow(x, Kernel.abs(n))} |> reduce()
    end
  end

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(integer, rational) :: float
  def pow_real(n, {x, y}) do
    case x >= 0 do
      true -> root(Integer.pow(n, x), y)
      false -> 1 / root(Integer.pow(n, Kernel.abs(x)), y)
    end
  end

  @doc false
  defp root(p, q), do: nth_root(q, p)

  defp nth_root(n, x, precision \\ 1.0e-5) do
    f = fn prev -> ((n - 1) * prev + x / :math.pow(prev, n - 1)) / n end
    fixed_point(f, x, precision, f.(x))
  end

  defp fixed_point(_, guess, tolerance, next) when Kernel.abs(guess - next) < tolerance, do: next
  defp fixed_point(f, _, tolerance, next), do: fixed_point(f, next, tolerance, f.(next))

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(rational) :: rational
  def reduce({x, y}) do
    gcd = Integer.gcd(x, y)

    case y >= 0 do
      true -> {div(x, gcd), div(y, gcd)}
      false -> {-div(x, gcd), -div(y, gcd)}
    end
  end
end
