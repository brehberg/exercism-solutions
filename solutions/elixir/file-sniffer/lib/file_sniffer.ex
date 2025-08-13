defmodule FileSniffer do
  @moduledoc """
  Module to verify that an upload matches its media type.
  """

  @error_msg "Warning, file format and file extension do not match."

  @media_type_elf "application/octet-stream"
  @media_type_bmp "image/bmp"
  @media_type_png "image/png"
  @media_type_jpg "image/jpg"
  @media_type_gif "image/gif"

  @binary_signature_elf <<0x7F, 0x45, 0x4C, 0x46>>
  @binary_signature_bmp <<0x42, 0x4D>>
  @binary_signature_png <<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A>>
  @binary_signature_jpg <<0xFF, 0xD8, 0xFF>>
  @binary_signature_gif <<0x47, 0x49, 0x46>>

  @doc "Given an extension, return the expected media type"
  @spec type_from_extension(String.t()) :: String.t()
  def type_from_extension("exe"), do: @media_type_elf
  def type_from_extension("bmp"), do: @media_type_bmp
  def type_from_extension("png"), do: @media_type_png
  def type_from_extension("jpg"), do: @media_type_jpg
  def type_from_extension("gif"), do: @media_type_gif
  def type_from_extension(_extension), do: nil

  @doc "Given a binary file, return the expected media type"
  @spec type_from_binary(binary) :: String.t()
  def type_from_binary(<<@binary_signature_elf, _::binary>>), do: @media_type_elf
  def type_from_binary(<<@binary_signature_bmp, _::binary>>), do: @media_type_bmp
  def type_from_binary(<<@binary_signature_png, _::binary>>), do: @media_type_png
  def type_from_binary(<<@binary_signature_jpg, _::binary>>), do: @media_type_jpg
  def type_from_binary(<<@binary_signature_gif, _::binary>>), do: @media_type_gif
  def type_from_binary(_file_binary), do: nil

  @doc "Given an extension and a binary file, verify that the file matches the expected type"
  @spec verify(binary, String.t()) :: {:ok | :error, String.t()}
  def verify(file_binary, extension) do
    file_type = type_from_binary(file_binary)
    ext_type = type_from_extension(extension)
    if file_type != nil && file_type == ext_type, do: {:ok, file_type}, else: {:error, @error_msg}
  end
end
