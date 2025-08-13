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

-- Return the value of a color band
local color_code = function(color)
  for index, value in ipairs(colors) do
    if value == color then
      return index - 1
    end
  end
end

return {
  -- Calculate a resistance value from two colors
  value = function(colors)
    return color_code(colors[1]) * 10 + color_code(colors[2])
  end
}
