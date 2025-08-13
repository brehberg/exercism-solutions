export function convert(num: number) {
  const isDivisbleBy = (n: number): boolean => num % n == 0
  let sound: string = ""
  if (isDivisbleBy(3)) sound += "Pling"
  if (isDivisbleBy(5)) sound += "Plang"
  if (isDivisbleBy(7)) sound += "Plong"
  return sound || num.toString()
}
