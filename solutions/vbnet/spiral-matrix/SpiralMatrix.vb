Option Strict On
Imports System

Public Class SpiralMatrix
    'row and column indexes representing a position in the matrix
    Private Structure Position
        Dim row, col As Integer
    End Structure

    'sequence of directions to fill the matrix in clockwise order
    Private Enum Direction As Integer
        Right
        Down
        Left
        Up
    End Enum

    'GetMatrix returns a square matrix of a given size that is filled with natural numbers,
    'starting from 1 in the top-left corner, increasing in an inward, clockwise spiral order.
    Public Shared Function GetMatrix(ByVal size As Integer) As Integer(,)
        Dim spiral(size-1, size-1) As Integer
        Dim pos As New Position With {.row = 0, .col = 0}
        Dim dir As Direction = Direction.Right
        For value As Integer = 1 To size * size
            'update value at this position and check for valid next move
            spiral(pos.row, pos.col) = value
            Dim test = adjustPosition(pos, dir)
            If test.col >= size OrElse 'position beyond last column, turn down instead
               test.row >= size OrElse 'position beyond last row, turn left instead
               test.col < 0 OrElse     'position beyond first column, turn up instead
               test.row < 0 OrElse     'position beyond first row, turn right instead
               spiral(test.row, test.col) > 0 'position is filled, turn instead
                dir = nextDirection(dir)
            End If            
            pos = adjustPosition(pos, dir)
        Next        
        Return spiral
    End Function

    'adjustPosition returns a new position based on the current direction
    Private Shared Function adjustPosition(ByVal pos As Position, dir As Direction) As Position
        Select Case dir
            Case Direction.Right 
                pos.col += 1  'move right to next column
            Case Direction.Down
                pos.row += 1  'move down to next row
            Case Direction.Left
                pos.col -= 1  'move left to previous column
            Case Direction.Up
                pos.row -= 1  'move up to previous row
        End Select
        Return pos
    End Function

    Private Shared Function nextDirection(ByVal dir As Direction) As Direction
        Return CType((dir + 1) mod 4, Direction)
    End Function
End Class
