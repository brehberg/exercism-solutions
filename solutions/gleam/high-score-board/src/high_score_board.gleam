import gleam/dict.{type Dict}

pub type ScoreBoard =
  Dict(String, Int)

// 1. Create a new high score board
pub fn create_score_board() -> ScoreBoard {
  dict.from_list([#("The Best Ever", 1_000_000)])
}

// 2. Add players to a score board
pub fn add_player(
  score_board: ScoreBoard,
  player: String,
  score: Int,
) -> ScoreBoard {
  score_board |> dict.insert(player, score)
}

// 3. Remove players from a score board
pub fn remove_player(score_board: ScoreBoard, player: String) -> ScoreBoard {
  score_board |> dict.delete(player)
}

// 4. Increase a player's score
pub fn update_score(
  score_board: ScoreBoard,
  player: String,
  points: Int,
) -> ScoreBoard {
  case score_board |> dict.get(player) {
    Ok(score) -> score_board |> add_player(player, score + points)
    _ -> score_board
  }
}

// 5. Apply Monday bonus points
pub fn apply_monday_bonus(score_board: ScoreBoard) -> ScoreBoard {
  score_board |> dict.map_values(fn(_, score) { score + monday_bonus_points })
}

const monday_bonus_points: Int = 100
