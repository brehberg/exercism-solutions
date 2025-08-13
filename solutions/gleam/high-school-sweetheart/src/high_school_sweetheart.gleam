import gleam/list
import gleam/result
import gleam/string

pub fn first_letter(name: String) {
  name |> string.trim |> string.first |> result.unwrap("")
}

pub fn initial(name: String) {
  name |> first_letter |> string.uppercase <> "."
}

pub fn initials(full_name: String) {
  case full_name |> string.split(" ") |> list.map(initial) {
    [first, last] -> first <> " " <> last
    _ -> ""
  }
}

pub fn pair(full_name1: String, full_name2: String) {
  let one = full_name1 |> initials
  let two = full_name2 |> initials
  let label = one <> "  +  " <> two
  "
     ******       ******
   **      **   **      **
 **         ** **         **
**            *            **
**                         **
**     " <> label <> "     **
 **                       **
   **                   **
     **               **
       **           **
         **       **
           **   **
             ***
              *
"
}
