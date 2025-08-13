 class PascalsTriangle {
  List<List<int>> rows(int n) {
    List<List<int>> triange = [];
    List<int> row = [1];

    while (n > 0) {
      triange.add(row);
      row = _generateNextRow(row);
      n--;
    }
    return triange;
  }

  List<int> _generateNextRow(List<int> previous) {
    var nextRow = [1];
    for (var i = 1; i < previous.length; i++) {
      nextRow.add(previous[i-1] + previous[i]);
    }
    nextRow.add(1);
    return nextRow;
  }
}
