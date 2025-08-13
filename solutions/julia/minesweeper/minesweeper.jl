function annotate(input)
    # check for no rows or columns
    if length(input) == 0 || input == [""]
        return input
    end

    # create matrix of minefield
    minefield = permutedims(reduce(hcat, collect.(input)))
    rows = size(minefield)[1]
    cols = size(minefield)[2]
    cells = CartesianIndices(minefield)

    neighbors = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    # Traverse minefield to find mine and count mines around it
    for cell in cells
        # skip if cell is not a mine
        if minefield[cell] != '*'
            continue
        end

        for offest in neighbors
            neighbor = cell + CartesianIndex(offest)

            # skip if neighbor is not in bounds or is a mine
            if !checkbounds(Bool, minefield, neighbor) || minefield[neighbor] == '*'
                continue
            end

            # otherwise add 1 to neighbor value
            if minefield[neighbor] == ' '
                minefield[neighbor] = 48 # char = '0'
            end
            minefield[neighbor] += 1
        end
    end
    reduce(*, reshape(minefield, (rows, cols)), dims=2)[:]
end
