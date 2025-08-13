module LogLevels

let message (logLine: string) : string =
    let marker: string = "]:"
    let offset: int = logLine.IndexOf(marker) + marker.Length
    logLine.Substring(offset).Trim()

let logLevel (logLine: string) : string =
    let start: int = logLine.IndexOf("[") + 1
    let length: int = logLine.IndexOf("]") - start
    logLine.Substring(start, length).ToLower()

let reformat (logLine: string) : string =
    $"{message logLine} ({logLevel logLine})"
