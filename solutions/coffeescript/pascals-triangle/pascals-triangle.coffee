class PascalsTriangle
  rows: (num) ->
    triangle = []
    return triangle unless num > 0
    for i in [0..num-1]
      triangle.push @generateNextRow(triangle[i-1])
    return triangle

  generateNextRow: (prev = []) ->
    (prev[i-1] + prev[i] || 1) for i in [0..prev.length]

module.exports = PascalsTriangle
