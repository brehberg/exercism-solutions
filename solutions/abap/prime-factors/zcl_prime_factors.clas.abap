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

ENDCLASS.


CLASS zcl_prime_factors IMPLEMENTATION.

  METHOD factors.
    CHECK input > 1.

    DATA(number) = input.
    DATA(candidate) = 2.

    WHILE number DIV candidate >= candidate.
      IF number MOD candidate = 0.
        " found a factor, add to list and check remaining
        APPEND candidate TO result.
        number = number DIV candidate.
      ELSE.
        " this is not a factor, increment and try again
        candidate += 1.
      ENDIF.
    ENDWHILE.

    " remaining number is indivisible, add to factor list
    APPEND number TO result.
  ENDMETHOD.

ENDCLASS.
