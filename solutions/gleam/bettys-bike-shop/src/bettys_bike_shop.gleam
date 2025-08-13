import gleam/int
import gleam/float as f
import gleam/string.{append}

// Convert pence to pounds
pub fn pence_to_pounds(pence: Int) -> Float {
  int.to_float(pence) /. 100.0
}

// Format the price for display on the website
pub fn pounds_to_string(pounds: Float) -> String {
  append(to: "£", suffix: f.to_string(pounds))
}
