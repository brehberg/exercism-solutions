const workHoursPerDay = 8;
const billableDaysPerMonth = 22;
/**
 * The day rate, given a rate per hour
 *
 * @param {number} ratePerHour
 * @returns {number} the rate per day
 */
export function dayRate(ratePerHour) {
  return ratePerHour * workHoursPerDay;
}

/**
 * Calculates the number of days in a budget, rounded down
 *
 * @param {number} budget: the total budget
 * @param {number} ratePerHour: the rate per hour
 * @returns {number} the number of days
 */
export function daysInBudget(budget, ratePerHour) {
  return Math.floor(budget / dayRate(ratePerHour));
}

/**
 * The month rate, given a rate per hour and monthly discount
 *
 * @param {number} ratePerHour
 * @param {number} discount: for example 20% written as 0.2
 * @returns {number} the rate per day
 */
function monthRate(ratePerHour, discount) {
  return dayRate(ratePerHour) * billableDaysPerMonth * (1 - discount);
}

/**
 * Calculates the discounted rate for large projects, rounded up
 *
 * @param {number} ratePerHour
 * @param {number} numDays: number of days the project spans
 * @param {number} discount: for example 20% written as 0.2
 * @returns {number} the rounded up discounted rate
 */
export function priceWithMonthlyDiscount(ratePerHour, numDays, discount) {
  const numMonths = Math.floor(numDays / billableDaysPerMonth);
  const priceForMonths = monthRate(ratePerHour, discount) * numMonths;
  const remainingDays = numDays - numMonths * billableDaysPerMonth;  
  return Math.ceil(priceForMonths + remainingDays * dayRate(ratePerHour));
}

