fun isLeapYear year =
		let val isDivisibleBy = fn (n) => year mod n = 0
		in (isDivisibleBy 4)
			andalso not (isDivisibleBy 100)
			orelse (isDivisibleBy 400)
		end