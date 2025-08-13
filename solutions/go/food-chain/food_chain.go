package foodchain

import "strings"

type animal int

const (
	fly animal = iota
	spider
	bird
	cat
	dog
	goat
	cow
	horse
)

var chorus = map[animal]string{
	fly:    "a fly.",
	spider: "a spider.\nIt wriggled and jiggled and tickled inside her.",
	bird:   "a bird.\nHow absurd to swallow a bird!",
	cat:    "a cat.\nImagine that, to swallow a cat!",
	dog:    "a dog.\nWhat a hog, to swallow a dog!",
	goat:   "a goat.\nJust opened her throat and swallowed a goat!",
	cow:    "a cow.\nI don't know how she swallowed a cow!",
	horse:  "a horse.\nShe's dead, of course!",
}

var finale = map[animal]string{
	fly:    "I don't know why she swallowed the fly. Perhaps she'll die.",
	spider: "She swallowed the spider to catch the fly.",
	bird:   "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.",
	cat:    "She swallowed the cat to catch the bird.",
	dog:    "She swallowed the dog to catch the cat.",
	goat:   "She swallowed the goat to catch the dog.",
	cow:    "She swallowed the cow to catch the goat.",
}

func Verse(v int) string {
	var sb strings.Builder

	sb.WriteString("I know an old lady who swallowed ")
	sb.WriteString(chorus[animal(v-1)])
	if v != len(chorus) {
		for i := v; i > 0; i-- {
			sb.WriteString("\n")
			sb.WriteString(finale[animal(i-1)])
		}
	}

	return sb.String()
}

func Verses(start, end int) string {
	var sb strings.Builder

	for i := start; i < end; i++ {
		sb.WriteString(Verse(i))
		sb.WriteString("\n\n")
	}
	sb.WriteString(Verse(end))

	return sb.String()
}

func Song() string {
	return Verses(1, 8)
}
