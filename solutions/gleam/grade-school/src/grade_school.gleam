import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string

pub type School =
  Dict(Int, List(String))

pub fn create() -> School {
  dict.new()
}

pub fn roster(school: School) -> List(String) {
  school |> dict.values |> list.flatten
}

pub fn add(
  to school: School,
  student student: String,
  grade grade: Int,
) -> Result(School, Nil) {
  case school |> roster |> list.contains(student) {
    True -> Error(Nil)
    _ -> Ok(add_student_and_sort(school, grade, student))
  }
}

fn add_student_and_sort(school: School, grade: Int, new: String) -> School {
  let existing = school |> dict.get(grade) |> result.unwrap(or: [])
  let students = [new, ..existing] |> list.sort(by: string.compare)
  dict.insert(into: school, for: grade, insert: students)
}

pub fn grade(school: School, desired_grade: Int) -> List(String) {
  school |> dict.get(desired_grade) |> result.unwrap(or: [])
}
