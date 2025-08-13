Class {
	#name : #Minesweeper,
	#superclass : #Object,
	#category : #'Exercise@Minesweeper'
}

Minesweeper >> annotateMinefield: input [

    minefield := input deepCopy.

    minefield withIndexDo: [ :row :r |
        row withIndexDo: [ :cell :c |
            cell isSpaceSeparator ifTrue: [
                "Annotate specific cell in the minefield based on adjacent mines"
                n := self countMines: input row:r col:c.
                n > 0 ifTrue: [row at: c put: n asCharacterDigit]. 
            ].
        ].
    ].

    ^minefield
]

Minesweeper >> countMines: minefield row: r col: c [

    neighbors := self getNeighbors: minefield row:r col:c.
    
    mines := neighbors select: [:neighbor |    
        "Count adjacent mines"
        ((minefield at: (neighbor at: 1)) at: (neighbor at: 2)) = $* ].

    ^mines size
]

Minesweeper >> getNeighbors: minefield row: r col: c [
    
    deltas := #((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)).
    neighbors := OrderedCollection new.
    
    deltas do: [:delta |
        | row col |
        row := r + delta first.
        col := c + delta second.        
        "Check boundaries of the minefield"
        (row between: 1 and: minefield size) ifTrue: [
            (col between: 1 and: (minefield at: row) size) ifTrue: [
                neighbors add: {row. col}
            ]
        ].
    ].

    ^neighbors
]