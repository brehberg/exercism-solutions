Class {
	#name : #RomanNumerals,
	#superclass : #Object,
	#category : #'Exercise@RomanNumerals'
}

RomanNumerals >> romanNumber: n [
    n >= 1000 ifTrue: [^ 'M', (self romanNumber: n - 1000)].
    n >= 900  ifTrue: [^ 'C', (self romanNumber: n + 100)].
    n >= 500  ifTrue: [^ 'D', (self romanNumber: n - 500)].
    n >= 400  ifTrue: [^ 'C', (self romanNumber: n + 100)].
    n >= 100  ifTrue: [^ 'C', (self romanNumber: n - 100)].
    n >= 90   ifTrue: [^ 'X', (self romanNumber: n + 10)].
    n >= 50   ifTrue: [^ 'L', (self romanNumber: n - 50)].
    n >= 40   ifTrue: [^ 'X', (self romanNumber: n + 10)].
    n >= 10   ifTrue: [^ 'X', (self romanNumber: n - 10)].
    n >= 9    ifTrue: [^ 'I', (self romanNumber: n + 1)].
    n >= 5    ifTrue: [^ 'V', (self romanNumber: n - 5)].
    n >= 4    ifTrue: [^ 'I', (self romanNumber: n + 1)].
    n >= 1    ifTrue: [^ 'I', (self romanNumber: n - 1)].
    n <= 0    ifTrue: [^ ''].
]
