return {
  to_roman = function(n)
    remaining = n
    roman = ""

    local function cypher(value, unit, five, full)
      local function add_literals(multi, pattern)
        while (remaining >= value * multi) do
          roman = roman .. pattern
          remaining = remaining - value * multi          
        end
      end

      add_literals(9, unit .. full)
      add_literals(5, five)
      add_literals(4, unit .. five)
      add_literals(1, unit)
    end

    cypher(1000, "M", "", "")
    cypher(100, "C", "D", "M")
    cypher(10, "X", "L", "C")
    cypher(1, "I", "V", "X")
    return roman
  end
}
