local TwoFer = {}

function TwoFer.two_fer(name)
    local output = 'One for you, one for me.'
    return name and output:gsub('you', name) or output
end

return TwoFer
