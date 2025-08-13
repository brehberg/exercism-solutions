export function isLeap(year: number): boolean {
  const isDivisbleBy = (n: number): boolean => { return year % n == 0 }
  return isDivisbleBy(4) && !isDivisbleBy(100) || isDivisbleBy(400)
}
