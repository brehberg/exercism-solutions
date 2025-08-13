Class {
    #name : #Leap,
    #superclass : #Object,
    #category : #'Exercise@Leap'
}

Leap >> leapYear: year [
    ^(year isDivisibleBy: 4) and: [
        (year isDivisibleBy: 100) not or: [
            (year isDivisibleBy: 400)
        ]
    ]
]
