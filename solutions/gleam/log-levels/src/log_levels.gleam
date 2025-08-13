import gleam/string as str

pub fn message(log_line: String) -> String {
  case log_line {
    "[INFO]:" <> msg | "[WARNING]:" <> msg | "[ERROR]:" <> msg -> str.trim(msg)
    _ -> log_line
  }
}

pub fn log_level(log_line: String) -> String {
  case log_line {
    "[INFO]:" <> _ -> "info"
    "[WARNING]:" <> _ -> "warning"
    "[ERROR]:" <> _ -> "error"
    _ -> ""
  }
}

pub fn reformat(log_line: String) -> String {
  message(log_line) <> " (" <> log_level(log_line) <> ")"
}
