local TwoFer = {}

function TwoFer.two_fer(name)
    local output = 'One for you, one for me.'
    if name then output = string.gsub(output, 'you', name) end
    return output
end

return TwoFer
