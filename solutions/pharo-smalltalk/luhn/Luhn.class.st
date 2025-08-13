Class {
	#name : #Luhn,
	#superclass : #Object,
	#category : #'Exercise@Luhn'
}
{ #category : #private }
Luhn >> formatToLuhn: digitOnlyString [
	
	digitOnlyString size - 1  to: 1 by: -2 do: [:index |
		digitOnlyString at: index put: (self luhnDigitFor: (digitOnlyString at: index))
		]
]
{ #category : #private }
Luhn >> isDigitsOrSpaces: aString [
	^ aString allSatisfy: [ :each | each isDigit or: [ each isSeparator ] ]
]
{ #category : #private }
Luhn >> isValidLuhn: digitOnlyString [
	digitOnlyString size > 1 ifFalse: [ ^ false ].
	self formatToLuhn: digitOnlyString.
	^ digitOnlyString asNumber digitSum isDivisibleBy: 10
]
{ #category : #private }
Luhn >> luhnDigitFor: aDigitChar [
	^ (aDigitChar digitValue * 2) digitSum asCharacterDigit	 
	
]
{ #category : #api }
Luhn >> validValue: aString [ 
	
	^ (self isDigitsOrSpaces: aString) and: [
		self isValidLuhn: (aString select: #isDigit) ]
]
