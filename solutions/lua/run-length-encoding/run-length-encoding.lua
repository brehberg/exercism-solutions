local function encode_chunk(num, char)
  return num == 1 and char or num .. char
end

return {
  -- Generates a string where consecutive elements are a count and data value.
  encode = function(s)
    if s == '' then return '' end
    local out = ''

    local prev = s:sub(1, 1)
    local count = 1
    for n=2, s:len() do
      if s:sub(n,n) == prev then
        count = count + 1
      else
        out = out .. encode_chunk(count, prev)
        prev = s:sub(n, n)
        count = 1
      end
    end

    return out .. encode_chunk(count, prev)
  end,

  -- It should also be able to reconstruct the data into its original form.
  decode = function(s)
    if s == '' then return '' end
    local out = ''

    for match in s:gmatch('%d*.') do
      local last = match:len()
      local char = match:sub(last, last)
      local num = last == 1 and 1 or match:sub(1, last-1)
      out = out .. string.rep(char, num)
    end

    return out
  end
}
