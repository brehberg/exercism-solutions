return {
  color_code = function(color)
    -- Better Be Right Or Your Great Big Values Go Wrong
    local colors = {
      "black",
      "brown",
      "red",
      "orange",
      "yellow",
      "green",
      "blue",
      "violet",
      "grey",
      "white"
    }

    for index, value in ipairs(colors) do
      if value == color then
        return index - 1
      end
    end
  end
}
