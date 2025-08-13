proc reverse*(input: string): string =
  for c in input:
    result = c & result
