Imports System.Collections.Generic

Public Module MatchingBrackets
    Private ReadOnly matches As New Dictionary(Of Char, Char) _
        From {{"[", "]"}, {"{", "}"}, {"(", ")"}}

    ' Checks that all the brackets and braces in the string are matched correctly, 
    ' and nested correctly
    Public Function IsPaired(ByVal input As String) As Boolean
        Dim closerNeeded As New Stack(Of Char)
        Dim closer As Char = Nothing

        For Each c As Char In input
            If matches.TryGetValue(c, closer) Then
                ' opening bracket was found, add matching closing value to the stack
                closerNeeded.Push(closer)
            ElseIf matches.ContainsValue(c) Then
                ' closing bracket was found, is it the next expected value on stack?
                If Not closerNeeded.Any() Then Return False
                If closerNeeded.Pop() <> c Then Return False
            End If
        Next

        Return Not closerNeeded.Any()
    End Function
End Module