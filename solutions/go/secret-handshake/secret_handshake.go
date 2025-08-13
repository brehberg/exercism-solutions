package secret

func Handshake(code uint) (actions []string) {
	actions = checkCode(code, 0b1, "wink", actions)
	actions = checkCode(code, 0b10, "double blink", actions)
	actions = checkCode(code, 0b100, "close your eyes", actions)
	actions = checkCode(code, 0b1000, "jump", actions)
	checkCode(code, 0b10000, "Reverse", actions)
	return
}

func checkCode(input, code uint, event string, acts []string) []string {
	if input&code != code {
		return acts
	}
	if event != "Reverse" {
		return append(acts, event)
	}
	for left, right := 0, len(acts)-1; left < right; left, right = left+1, right-1 {
		acts[left], acts[right] = acts[right], acts[left]
	}
	return acts
}
