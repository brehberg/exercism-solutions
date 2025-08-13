defmodule ComplexNumbers do
  import Kernel, except: [div: 2]

  @typedoc """
  In this module, complex numbers are represented as a tuple-pair containing the real and
  imaginary parts.
  For example, the real number `1` is `{1, 0}`, the imaginary number `i` is `{0, 1}` and
  the complex number `4+3i` is `{4, 3}'.
  """
  @type complex :: {float, float}

  @doc """
  Return the real part of a complex number
  """
  @spec real(z :: complex) :: float
  def real({a, _}), do: a

  @doc """
  Return the imaginary part of a complex number
  """
  @spec imaginary(z :: complex) :: float
  def imaginary({_, b}), do: b

  @doc """
  Multiply two complex numbers, or a real and a complex number
  """
  @spec mul(z :: complex | float, y :: complex | float) :: complex
  def mul(x, {c, d}) when is_number(x), do: mul({x, 0}, {c, d})
  def mul({a, b}, y) when is_number(y), do: mul({a, b}, {y, 0})

  def mul({a, b}, {c, d}) do
    {a * c - b * d, b * c + a * d}
  end

  @doc """
  Add two complex numbers, or a real and a complex number
  """
  @spec add(z :: complex | float, y :: complex | float) :: complex
  def add(x, {c, d}) when is_number(x), do: add({x, 0}, {c, d})
  def add({a, b}, y) when is_number(y), do: add({a, b}, {y, 0})

  def add({a, b}, {c, d}) do
    {a + c, b + d}
  end

  @doc """
  Subtract two complex numbers, or a real and a complex number
  """
  @spec sub(z :: complex | float, y :: complex | float) :: complex
  def sub(x, {c, d}) when is_number(x), do: sub({x, 0}, {c, d})
  def sub({a, b}, y) when is_number(y), do: sub({a, b}, {y, 0})

  def sub({a, b}, {c, d}) do
    {a - c, b - d}
  end

  @doc """
  Divide two complex numbers, or a real and a complex number
  """
  @spec div(z :: complex | float, y :: complex | float) :: complex
  def div(x, {c, d}) when is_number(x), do: div({x, 0}, {c, d})
  def div({a, b}, y) when is_number(y), do: div({a, b}, {y, 0})

  def div({a, b}, {c, d}) do
    {(a * c + b * d) / (c * c + d * d), (b * c - a * d) / (c * c + d * d)}
  end

  @doc """
  Absolute value of a complex number
  """
  @spec abs(z :: complex) :: float
  def abs({a, b}) do
    :math.sqrt(a * a + b * b)
  end

  @doc """
  Conjugate of a complex number
  """
  @spec conjugate(z :: complex) :: complex
  def conjugate({a, b}) do
    {a, -b}
  end

  @doc """
  Exponential of a complex number
  """
  @spec exp(z :: complex) :: complex
  def exp({a, b}) do
    {:math.exp(a) * :math.cos(b), :math.exp(a) * :math.sin(b)}
  end
end
