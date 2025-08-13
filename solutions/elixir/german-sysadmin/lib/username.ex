defmodule Username do
  def sanitize([]), do: ''
  def sanitize(username) do
    case username do
      [?ä | rest] -> 'ae' ++ sanitize(rest)
      [?ö | rest] -> 'oe' ++ sanitize(rest)
      [?ü | rest] -> 'ue' ++ sanitize(rest)
      [?ß | rest] -> 'ss' ++ sanitize(rest)
      [?_ | rest] -> '_' ++ sanitize(rest)
      [char | rest] when char not in ?a..?z -> sanitize(rest)
      [char | rest] -> [char | sanitize(rest)]
    end
  end
end
