pub fn append(first first: List(a), second second: List(a)) -> List(a) {
  foldr(over: first, from: second, with: fn(acc, item) { [item, ..acc] })
}

pub fn concat(lists: List(List(a))) -> List(a) {
  foldl(over: lists, from: [], with: append)
}

pub fn filter(list: List(a), function: fn(a) -> Bool) -> List(a) {
  foldr(over: list, from: [], with: fn(acc, item) {
    case function(item) {
      False -> acc
      _ -> [item, ..acc]
    }
  })
}

pub fn length(list: List(a)) -> Int {
  foldl(over: list, from: 0, with: fn(count, _) { count + 1 })
}

pub fn map(list: List(a), function: fn(a) -> b) -> List(b) {
  foldr(over: list, from: [], with: fn(acc, item) { [function(item), ..acc] })
}

pub fn foldl(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  case list {
    [] -> initial
    [head, ..rest] -> foldl(rest, function(initial, head), function)
  }
}

pub fn foldr(
  over list: List(a),
  from initial: b,
  with function: fn(b, a) -> b,
) -> b {
  foldl(over: reverse(list), from: initial, with: function)
}

pub fn reverse(list: List(a)) -> List(a) {
  foldl(over: list, from: [], with: fn(acc, item) { [item, ..acc] })
}
