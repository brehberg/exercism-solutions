return {
  to_roman = function(n)
    remaining = n
    value = 1000
    roman = ""

    local function cypher(unit, five, full)
      local function add_literals(multi, pattern)
        while (remaining >= multi * value) do
          roman = roman .. pattern
          remaining = remaining - multi * value
        end
      end

      add_literals(9, unit .. full)
      add_literals(5, five)
      add_literals(4, unit .. five)
      add_literals(1, unit)
      value = value / 10
    end

    cypher("M", "", "")
    cypher("C", "D", "M")
    cypher("X", "L", "C")
    cypher("I", "V", "X")
    return roman
  end
}
