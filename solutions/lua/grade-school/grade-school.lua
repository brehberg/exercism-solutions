local School = {}

function School:new()
    local newObj = {students = {}}
    self.__index = self
    return setmetatable(newObj, self)
end

function School:add(name, grade)
    if (not self.students[grade]) then
        self.students[grade] = {}
    end
    table.insert(self.students[grade], name)
    table.sort(self.students[grade])
end

function School:grade(grade)
    return self.students[grade] or {}
end

function School:roster()
    return self.students
end

return School
