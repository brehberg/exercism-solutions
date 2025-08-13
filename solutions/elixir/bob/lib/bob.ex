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
      is_nothing?(input) -> "Fine. Be that way!"
      is_question?(input) and is_yell?(input) -> "Calm down, I know what I'm doing!"
      is_yell?(input) -> "Whoa, chill out!"
      is_question?(input) -> "Sure."
      true -> "Whatever."
    end
  end

  @doc false
  defp is_yell?(str), do: String.upcase(str) == str and String.match?(str, ~r/[A-ZУХОДИ]/)
  defp is_question?(str), do: String.trim(str) |> String.ends_with?("?")
  defp is_nothing?(str), do: String.trim(str) == ""
end
