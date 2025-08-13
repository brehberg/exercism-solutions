local triangle = {}

function triangle.kind(a, b, c)
    assert(a > 0 and b > 0 and c > 0, "Input Error")
    assert(a + b > c and b + c > a and c + a > b, "Input Error")

    if a == b and b == c then
        return "equilateral"
    elseif a == b or b == c or c == a then
        return "isosceles"
    else
        return "scalene"
    end
end

return triangle
