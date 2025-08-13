defmodule Newsletter do
  @doc """
  Read email addresses from a file
  """
  @spec read_emails(Path.t()) :: [String.t()]
  def read_emails(path) do
    File.read!(path) |> String.split("\n", trim: true)
  end

  @doc """
  Open a log file for writing
  """
  @spec open_log(Path.t()) :: File.io_device()
  def open_log(path) do
    File.open!(path, [:write])
  end

  @doc """
  Log a sent email
  """
  @spec log_sent_email(File.io_device(), String.t()) :: :ok
  def log_sent_email(pid, email) do
    IO.puts(pid, email)
  end

  @doc """
  Close the log file
  """
  @spec close_log(File.io_device()) :: :ok
  def close_log(pid) do
    File.close(pid)
  end

  @doc """
  Send the newsletter
  """
  @spec send_newsletter(Path.t(), Path.t(), (String.t() -> :ok)) :: :ok
  def send_newsletter(emails_path, log_path, send_fun) do
    log_file = open_log(log_path)

    read_emails(emails_path) |> Enum.map(&send_and_log(&1, send_fun, log_file))

    close_log(log_file)
  end

  @doc false
  @spec send_and_log(String.t(), (String.t() -> :ok), File.io_device()) :: :ok
  defp send_and_log(email, send_fun, log_file) do
    if send_fun.(email) == :ok, do: log_sent_email(log_file, email)
  end
end
