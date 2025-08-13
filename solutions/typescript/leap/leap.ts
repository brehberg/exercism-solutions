export function isLeap(year: number): boolean {
  const is_divisble = (n: number): boolean => { return year % n == 0 }

  return is_divisble(4) && !is_divisble(100) || is_divisble(400)
}
