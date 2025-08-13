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
    trimmed = String.trim(input)

    cond do
      nothing?(trimmed) -> "Fine. Be that way!"
      yell_question?(trimmed) -> "Calm down, I know what I'm doing!"
      yell?(trimmed) -> "Whoa, chill out!"
      question?(trimmed) -> "Sure."
      true -> "Whatever."
    end
  end

  @doc false
  defp question?(str), do: String.ends_with?(str, "?")
  defp yell?(str), do: String.upcase(str) == str and String.downcase(str) != str
  defp yell_question?(str), do: question?(str) and yell?(str)
  defp nothing?(str), do: str == ""
end
