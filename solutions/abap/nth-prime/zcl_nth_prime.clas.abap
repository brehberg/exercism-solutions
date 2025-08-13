CLASS zcl_nth_prime DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS prime
      IMPORTING
        input         TYPE i
      RETURNING
        VALUE(result) TYPE i
      RAISING
        cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES integers TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    CLASS-DATA starting_primes TYPE integers.
    METHODS determine_limit
      IMPORTING n            TYPE i
      RETURNING VALUE(limit) TYPE i.
    METHODS get_candidates
      IMPORTING limit       TYPE i
      RETURNING VALUE(list) TYPE integers.
    METHODS sieve
      IMPORTING list          TYPE integers
                primes        TYPE integers
      RETURNING VALUE(result) TYPE integers.
ENDCLASS.


CLASS zcl_nth_prime IMPLEMENTATION.

  METHOD prime.
    IF input < 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF input < 6.
      result = starting_primes[ input ].
      RETURN.
    ENDIF.

    DATA(initial) = get_candidates( determine_limit( input ) ).
    DATA(primes) = sieve( list = initial primes = VALUE #( ( 2 ) ) ).
    result = primes[ input ].
  ENDMETHOD.

  METHOD determine_limit.
    " prime(n) < n*(log n + log (log n)), for n >= 6
    " limit = floor( n * ( log( n ) + log( log( n ) ) ) ).

    " function ABAP.builtin.log is not available on Exercism
    " using below to find a limit with reasonable runtime instead
    limit = floor( n * sqrt( n ) ).
  ENDMETHOD.

  METHOD get_candidates.
    " initial candidate list is all odd numbers starting from 3
    list = VALUE #( FOR i = 3 THEN i + 2 WHILE i < limit ( i ) ).
  ENDMETHOD.

  METHOD sieve.
    " Sieve of Eratosthenes algorithm for finding only prime numbers
    IF lines( list ) = 1.
      result = VALUE #( BASE primes ( list[ 1 ] ) ).
      RETURN.
    ENDIF.

    DATA(next) = list[ 1 ].
    DATA(remaining) = VALUE integers( ( LINES OF list FROM 2 ) ).

    " remove all multiples of next prime from the remaining candidates
    LOOP AT remaining INTO DATA(value).
      CHECK value MOD next = 0.
      DELETE remaining.
    ENDLOOP.

    result = sieve( list = remaining primes = VALUE #( BASE primes ( next ) ) ).
  ENDMETHOD.

  METHOD class_constructor.
    starting_primes = VALUE #( ( 2 ) ( 3 ) ( 5 ) ( 7 ) ( 11 ) ).
  ENDMETHOD.

ENDCLASS.