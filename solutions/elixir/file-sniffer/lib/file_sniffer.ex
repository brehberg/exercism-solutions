defmodule FileSniffer do
  @doc """
  Given an extension, return the expected media type
  """
  @spec type_from_extension(String.t()) :: String.t()
  def type_from_extension("exe"), do: "application/octet-stream"
  def type_from_extension("bmp"), do: "image/bmp"
  def type_from_extension("png"), do: "image/png"
  def type_from_extension("jpg"), do: "image/jpg"
  def type_from_extension("gif"), do: "image/gif"
  def type_from_extension(extension), do: nil

  @doc """
  Given a binary file, return the expected media type
  """
  @spec type_from_binary(binary) :: String.t()
  def type_from_binary(<<0x7F, 0x45, 0x4C, 0x46, rest::binary>>), do: "application/octet-stream"
  def type_from_binary(<<0x42, 0x4D, rest::binary>>), do: "image/bmp"
  def type_from_binary(<<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, rest::binary>>), do: "image/png"
  def type_from_binary(<<0xFF, 0xD8, 0xFF, rest::binary>>), do: "image/jpg"
  def type_from_binary(<<0x47, 0x49, 0x46, rest::binary>>), do: "image/gif"
  def type_from_binary(file_binary), do: nil

  @doc """
  Given an extension and a binary file, verify that the file matches the expected type
  """
  @error_msg "Warning, file format and file extension do not match."
  @spec verify(binary, String.t()) :: {:ok | :error, String.t()}
  def verify(file_binary, extension) do
    file_type = type_from_binary(file_binary)
    ext_type = type_from_extension(extension)
    if file_type == ext_type, do: {:ok, file_type}, else: {:error, @error_msg}
  end
end
