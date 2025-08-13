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

function Character:new(name)
  local con_score = ability(roll_dice())
  return {
    name = name,
    strength = ability(roll_dice()),
    dexterity = ability(roll_dice()),
    constitution = con_score,
    intelligence = ability(roll_dice()),
    wisdom = ability(roll_dice()),
    charisma = ability(roll_dice()),
    hitpoints = 10 + modifier(con_score)
  }
end

return {
  Character = Character,
  ability = ability,
  roll_dice = roll_dice,
  modifier = modifier
}
