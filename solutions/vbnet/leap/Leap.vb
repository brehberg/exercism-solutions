Public Module Leap
    Public Function IsLeapYear(ByVal year As Integer) As Boolean
        Dim IsDivisibleBy = 
            Function(n As Integer) As Boolean
                Return (year Mod n) = 0
            End Function
        Return IsDivisibleBy(4) AndAlso 
            Not IsDivisibleBy(100) OrElse 
            IsDivisibleBy(400)
    End Function
End Module