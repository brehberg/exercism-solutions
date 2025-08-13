module InterestIsInteresting

let interestRate (balance: decimal): single =
    match balance with
    | b when b < 0.00m -> 3.213f
    | b when b < 1000.00m -> 0.5f
    | b when b < 5000.00m -> 1.621f
    | _ -> 2.475f

let interest (balance: decimal): decimal =
   balance * (decimal)(interestRate balance / 100f)

let annualBalanceUpdate(balance: decimal): decimal =
   balance + interest balance

let amountToDonate(balance: decimal) (taxFreePercentage: float): int =
   match balance with
   | b when b < 0.00m -> 0
   | _ -> (int)(balance * (decimal)(taxFreePercentage / 50.0))
