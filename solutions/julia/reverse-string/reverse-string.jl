"""
    myreverse(input::String)

Return the reverse of a given input string.

"""
myreverse(input::String) = input[end:-1:begin]
