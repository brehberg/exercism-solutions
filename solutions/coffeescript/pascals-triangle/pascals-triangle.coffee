class PascalsTriangle
  rows: (num) ->
    triangle = []
    row = [1]
    while num > 0
      triangle.push row
      nextRow = []
      nextRow.push (row[i-1] + row[i] || 1) for i in [0..row.length]
      row = nextRow
      num -= 1
    return triangle

module.exports = PascalsTriangle
