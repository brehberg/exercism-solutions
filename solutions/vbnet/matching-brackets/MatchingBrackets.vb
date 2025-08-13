Imports System.Collections.Generic

Public Module MatchingBrackets
    Dim matches As New Dictionary(Of Char, Char) From {{"[", "]"}, {"{", "}"}, {"(", ")"}}

    ' Checks that all the brackets and braces in the string are matched correctly, 
    ' and nested correctly
    Public Function IsPaired(ByVal input As String) As Boolean
        Dim closerNeeded As New Stack()
        Dim closer As Char = Nothing

        For Each c AS Char in input
            If matches.TryGetValue(c, closer) Then
                ' opening bracket was found, add matching closing value to the stack
                closerNeeded.Push(closer)
            ElseIf matches.ContainsValue(c) Then
                ' closing bracket was found, is it the next expected value on stack?
                If closerNeeded.Count = 0 Then
                    Return False
                ElseIf closerNeeded.Pop() <> c Then
                    Return False
                End IF
            End If
        Next

        Return closerNeeded.Count = 0
    End Function
End Module