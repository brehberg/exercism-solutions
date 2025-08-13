pub type TreasureChest(contents) {
  TreasureChest(String, contents)
}

pub type UnlockResult(contents) {
  Unlocked(contents)
  WrongPassword
}

pub fn get_treasure(
  chest: TreasureChest(treasure),
  password: String,
) -> UnlockResult(treasure) {
  case chest {
    TreasureChest(key, treasure) if key == password -> Unlocked(treasure)
    _ -> WrongPassword
  }
}
