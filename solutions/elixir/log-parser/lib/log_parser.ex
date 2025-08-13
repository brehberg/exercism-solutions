defmodule LogParser do
  @moduledoc """
  Functions to clean up the organization's archived log files.
  """

  @doc """
  Identify garbled log lines

  To be considered valid a line should begin with one of the following strings:
    [DEBUG]  [INFO]  [WARNING]  [ERROR]
  """
  @spec valid_line?(String.t()) :: boolean
  def valid_line?(line), do: line =~ ~r/^\[DEBUG|INFO|WARNING|ERROR\]/

  @doc """
  Split the log line

  Any string that has a first character of <, a last character of >, and any combination
  of the following characters ~, *, =, and - in between can be used as a separator.
  """
  @spec split_line(String.t()) :: list(String.t())
  def split_line(line), do: Regex.split(~r/<[~*=-]*>/, line)

  @doc """
  Remove artifacts from log

  Remove all occurrence end-of-line text (case-insensitive) and return a clean log line.
  """
  @spec remove_artifacts(String.t()) :: String.t()
  def remove_artifacts(line), do: Regex.replace(~r/end-of-line\d+/i, line, "")

  @doc """
  Tag lines with user names

  Log lines that refer to users always contain the string "User", followed by one or more
  whitespace characters, and then a user name. For lines that contain the string "User",
  prefix the line with [USER] followed by the user name. Other lines remain unchanged.
  """
  @spec tag_with_user_name(String.t()) :: String.t()
  def tag_with_user_name(line) do
    username = Regex.run(~r/User\s+(\S+)\s?/i, line, capture: :all_but_first)
    if username, do: "[USER] #{username} #{line}", else: line
  end
end
