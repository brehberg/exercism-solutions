defmodule PaintByNumber do
  # Calculate palette bit size
  def palette_bit_size(color_count), do: calc_bit_size(color_count, 0)

  defp calc_bit_size(colors, bits),
    do: if(colors <= Integer.pow(2, bits), do: bits, else: calc_bit_size(colors, bits + 1))

  # Create an empty picture
  def empty_picture(), do: <<>>

  # Create a test picture
  def test_picture(), do: <<0b00::2, 0b01::2, 0b10::2, 0b11::2>>

  # Prepend a pixel to a picture
  def prepend_pixel(picture, color_count, pixel_color_index) do
    pixel_size = palette_bit_size(color_count)
    <<pixel_color_index::size(pixel_size), picture::bitstring>>
  end

  # Get the first pixel from a picture
  def get_first_pixel(<<>>, _), do: nil

  def get_first_pixel(picture, color_count) do
    pixel_size = palette_bit_size(color_count)
    <<first::size(pixel_size), _rest::bitstring>> = picture
    first
  end

  # Drop the first pixel from a picture
  def drop_first_pixel(<<>>, _), do: <<>>

  def drop_first_pixel(picture, color_count) do
    pixel_size = palette_bit_size(color_count)
    <<_first::size(pixel_size), rest::bitstring>> = picture
    rest
  end

  # Concatenate two pictures
  def concat_pictures(picture1, picture2), do: <<picture1::bitstring, picture2::bitstring>>
end
