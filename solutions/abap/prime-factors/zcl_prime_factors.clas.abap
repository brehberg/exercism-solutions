CLASS zcl_prime_factors DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES integertab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    METHODS factors
      IMPORTING
        input         TYPE int8
      RETURNING
        VALUE(result) TYPE integertab.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_factors
      IMPORTING
        next           TYPE int8
        number         TYPE int8
        primes         TYPE integertab OPTIONAL
      RETURNING
        VALUE(factors) TYPE integertab.

ENDCLASS.


CLASS zcl_prime_factors IMPLEMENTATION.

  METHOD factors.
    CHECK input > 1.
    result = find_factors( next = 2 number = input ).
  ENDMETHOD.

  METHOD find_factors.
    factors = COND #(
      " remaining number is indivisible, add to factor list
      WHEN number DIV next < next
      THEN VALUE #( BASE primes ( CONV #( number ) ) )
      " next is a factor, add to list and check remaining
      WHEN number MOD next = 0
      THEN find_factors( next = next
        number = number DIV next
        primes = VALUE #( BASE primes ( CONV #( next ) ) ) )
      " next is not a factor, increment and try again
      ELSE find_factors( next = next + 1
        number = number primes = primes ) ).
  ENDMETHOD.

ENDCLASS.
