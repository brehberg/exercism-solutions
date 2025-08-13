import Foundation

func isValidLuhn(_ number: String) -> Bool {
  let clean = number.replacingOccurrences(of: " ", with: "")
  guard clean.count > 1 else { return false }

  var sum = 0
  var odd = true

  for c in clean.reversed() {
    guard let d = Int(String(c)) else { return false }
    sum += odd ? d : (d < 5 ? d * 2 : d * 2 - 9)
    odd.toggle()
  }
  return sum.isMultiple(of: 10)
}
