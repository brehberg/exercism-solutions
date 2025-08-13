package parsinglogfiles

import "regexp"

// IsValidLine returns of false if it identifies garbled log lines.
//
// To be considered valid a line should begin with one of the following strings:
//
//	[TRC]  [DBG]  [INF]  [WRN]  [ERR]  [FTL]
func IsValidLine(text string) bool {
	re := regexp.MustCompile(`^\[(?:TRC|DBG|INF|WRN|ERR|FTL)\]`)
	return re.MatchString(text)
}

// SplitLogLine returns a slice of strings split on valid separators.
//
//	Any string that has a first character of <, a last character of >, and any combination
//	of the following characters ~, *, =, and - in between can be used as a separator.
func SplitLogLine(text string) []string {
	re := regexp.MustCompile(`<[~*=-]*>`)
	return re.Split(text, -1)
}

// CountQuotedPasswords returns the number of references to passwords in quoted text.
//
// Identified log lines where the string "password", which may be in any combination of upper or
// lower case, is surrounded by quotation marks. Each line contains at most two quotation marks.
func CountQuotedPasswords(lines []string) int {
	re := regexp.MustCompile(`".*(?i:password).*"`)
	passwordCount := 0
	for _, line := range lines {
		if re.MatchString(line) {
			passwordCount += 1
		}
	}
	return passwordCount
}

// RemoveEndOfLineText returns a clean log line after removing all occurrence end-of-line text.
func RemoveEndOfLineText(text string) string {
	re := regexp.MustCompile(`(?i:end-of-line\d+)`)
	return re.ReplaceAllString(text, "")
}

// TagWithUserName returns a slice of log lines tagged with identified user names.
//
// Log lines that refer to users always contain the string "User", followed by one or more
// whitespace characters, and then a user name. For lines that contain the string "User",
// prefix the line with [USR] followed by the user name. Other lines remain unchanged.
func TagWithUserName(lines []string) []string {
	re := regexp.MustCompile(`User\s+(\S+)\s?`)
	for i, line := range lines {
		username := re.FindStringSubmatch(line)
		if username != nil {
			lines[i] = "[USR] " + username[1] + " " + line
		}
	}
	return lines
}
