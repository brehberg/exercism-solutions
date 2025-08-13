local Character = {}

local function ability(scores)
  table.sort(scores, function(a,b) return a > b end)
  return scores[1] + scores[2] + scores[3]
end

local function roll_dice()
  math.randomseed(os.time())
  local rolls = {}
  for i=1,4 do
    rolls[i] = math.random(1,6)
  end
  return rolls
end

local function modifier(input)
  return math.floor((input - 10) / 2)
end

local abilities = {
  "strength", "dexterity", "constitution", 
  "intelligence", "wisdom", "charisma"
}

function Character:new(name)
  local newObj = { name = name }    
  for _, a in ipairs(abilities) do
    newObj[a] = ability(roll_dice())
  end
  newObj.hitpoints = 10 + modifier(newObj.constitution)

  self.__index = self
  return setmetatable(newObj, self)
end

return {
  Character = Character,
  ability = ability,
  roll_dice = roll_dice,
  modifier = modifier
}
