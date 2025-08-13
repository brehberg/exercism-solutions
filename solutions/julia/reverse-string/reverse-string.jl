using Unicode
"""
    myreverse(input::String)

Return the reverse of a given input string.

"""
function myreverse(input::String)
    array = collect(graphemes(input))
    return join(array[end:-1:begin])
end
