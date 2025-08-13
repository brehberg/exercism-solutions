defmodule Username do
  def sanitize([]), do: ''
  def sanitize(username) do
    case username do
      [char|rest] when char == ?_ -> '_' ++ sanitize(rest)
      [char|rest] when char == ?ä -> 'ae' ++ sanitize(rest)
      [char|rest] when char == ?ö -> 'oe' ++ sanitize(rest)
      [char|rest] when char == ?ü -> 'ue' ++ sanitize(rest)
      [char|rest] when char == ?ß -> 'ss' ++ sanitize(rest)
      [char|rest] when char < ?a or char > ?z -> sanitize(rest)
      [char|rest] -> [char | sanitize(rest)]
    end
  end
end
