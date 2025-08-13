class Minesweeper
  def initialize(@minefield : Array(String))
  end

  def annotate
    @minefield.map_with_index do |row, r|
      row.chars.map_with_index do |cell, c|
        cell.to_s.tr(" ", mine_count(r, c))
      end.join
    end
  end

  private def mine_count(r, c)
    ([r - 1, 0].max..[r + 1, @minefield.size - 1].min).sum do |r|
      ([c - 1, 0].max..[c + 1, @minefield[0].size - 1].min).count do |c|
        @minefield[r][c] == '*'
      end
    end.to_s.tr("0", " ")
  end
end
