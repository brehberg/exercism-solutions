class PhoneNumber {
  private var number: String
  init(_ phoneNumber: String) { number = phoneNumber }

  func clean() throws -> String {

    var digits = number.filter { $0.isNumber }
    if digits.count > 11 || digits.count < 10 { throw PhoneNumberError.invalidPhoneNumber }

    // all NANP-numbers share the same country code
    if digits.count == 11 {
      if digits[digits.startIndex] != "1" { throw PhoneNumberError.invalidPhoneNumber }
      digits.remove(at: digits.startIndex)
    }

    // area and exchange codes only start with digits from 2 through 9
    let areaCode = digits[digits.startIndex]
    let exchangeCode = digits[digits.index(digits.startIndex, offsetBy: 3)]
    if areaCode == "0" || areaCode == "1" { throw PhoneNumberError.invalidPhoneNumber }
    if exchangeCode == "0" || exchangeCode == "1" { throw PhoneNumberError.invalidPhoneNumber }

    return String(digits)
  }
}

enum PhoneNumberError: Error {
  case invalidPhoneNumber
}
