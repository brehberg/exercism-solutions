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
  case school |> roster |> list.contains(any: student) {
    True -> Error(Nil)
    _ -> Ok(school |> add_new_student_and_sort(grade, student))
  }
}

fn add_new_student_and_sort(
  to school: School,
  grade grade: Int,
  student new_student: String,
) -> School {
  let existing = case school |> dict.get(grade) {
    Ok(students) -> students
    _ -> []
  }
  let students = [new_student, ..existing] |> list.sort(by: string.compare)
  school |> dict.insert(for: grade, insert: students)
}

pub fn grade(school: School, desired_grade: Int) -> List(String) {
  school |> dict.get(desired_grade) |> result.unwrap(or: [])
}
