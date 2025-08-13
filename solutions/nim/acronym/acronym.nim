import strutils

proc abbreviate*(letter: string): string =
  for word in letter.split(AllChars - Letters - {'\''}):
    if word.len > 0: result &= word[0].toUpperAscii()
