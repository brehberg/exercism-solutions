defmodule Username do
  @doc """
  Sanitize existing usernames by removing everything but lowercase letters,
  allowing underscores, and substituting German characters
  """
  @spec sanitize(charlist) :: charlist
  def sanitize(username), do: do_sanitize(username, [])

  @spec do_sanitize(name :: charlist, clean :: charlist) :: charlist
  defp do_sanitize([], clean), do: clean
  defp do_sanitize([char | rest], clean) do
    case char do
      ?_ -> do_sanitize(rest, clean ++ '_')
      ?ä -> do_sanitize(rest, clean ++ 'ae')
      ?ö -> do_sanitize(rest, clean ++ 'oe')
      ?ü -> do_sanitize(rest, clean ++ 'ue')
      ?ß -> do_sanitize(rest, clean ++ 'ss')
      char when char in ?a..?z -> do_sanitize(rest, clean ++ [char])
      _ -> do_sanitize(rest, clean)
    end
  end
end
