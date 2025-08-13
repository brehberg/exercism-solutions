package airportrobot

import "fmt"

const helloMsg = "I can speak %s: %s"

type Greeter interface {
	LanguageName() string
	Greet(name string) string
}

func SayHello(name string, g Greeter) string {
	return fmt.Sprintf(helloMsg, g.LanguageName(), g.Greet(name))
}

type Italian struct {
}

func (greeter Italian) LanguageName() string {
	return "Italian"
}

func (greeter Italian) Greet(name string) string {
	return "Ciao " + name + "!"
}

type Portuguese struct {
}

func (greeter Portuguese) LanguageName() string {
	return "Portuguese"
}

func (greeter Portuguese) Greet(name string) string {
	return "Olá " + name + "!"
}
