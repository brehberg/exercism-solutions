class Minesweeper
  def self.annotate(*args) = new(*args).annotate

  def initialize(input)
    @minefield = input.map(&:chars)
    @rows = @minefield.length
    @cols = @minefield[0]&.length.to_i
  end

  def annotate
    minefield.each_with_index do |row, r|
      row.each_with_index do |cell, c|
        increment_neighbors(r, c) if cell == "*"
      end
    end
    minefield.map(&:join)
  end

  private

  attr_reader :minefield, :rows, :cols

  def increment_neighbors(row, col)
    [-1, 0, 1].each do |row_offset|
      [-1, 0, 1].each do |col_offset|
        increment_neighbor(row + row_offset, col + col_offset)
      end
    end
  end

  def increment_neighbor(row, col)
    return if row < 0 || row > @rows - 1
    return if col < 0 || col > @cols - 1

    value = minefield[row][col]
    return if value == "*"

    minefield[row][col] = value.to_i + 1
  end
end
