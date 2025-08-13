defmodule Bob do
  @moduledoc """
  Bob is a lackadaisical teenager. In conversation, his responses are very limited.
  Bob answers 'Sure.' if you ask him a question, such as "How are you?".
  He answers 'Whoa, chill out!' if you YELL AT HIM (in all capitals).
  He answers 'Calm down, I know what I'm doing!' if you yell a question at him.
  He says 'Fine. Be that way!' if you address him without actually saying anything.
  He answers 'Whatever.' to anything else.
  """
  @spec hey(String.t()) :: String.t()
  def hey(input) do
    cond do
      nothing?(input) -> "Fine. Be that way!"
      question?(input) and yell?(input) -> "Calm down, I know what I'm doing!"
      yell?(input) -> "Whoa, chill out!"
      question?(input) -> "Sure."
      true -> "Whatever."
    end
  end

  @doc false
  defp yell?(str), do: String.upcase(str) == str and String.match?(str, ~r/[A-ZУХОДИ]/)
  defp question?(str), do: String.trim(str) |> String.ends_with?("?")
  defp nothing?(str), do: String.trim(str) == ""
end
