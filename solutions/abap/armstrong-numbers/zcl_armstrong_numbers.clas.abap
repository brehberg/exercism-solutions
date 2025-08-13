CLASS zcl_armstrong_numbers DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS is_armstrong_number IMPORTING num           TYPE i
                                RETURNING VALUE(result) TYPE abap_bool.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      digit  TYPE i,
      digits TYPE STANDARD TABLE OF digit WITH EMPTY KEY.
    METHODS extract_digits
      IMPORTING !number       TYPE i
      RETURNING VALUE(digits) TYPE digits.

ENDCLASS.



CLASS zcl_armstrong_numbers IMPLEMENTATION.

  METHOD is_armstrong_number.
    " An Armstrong number is a number that is the sum of its own
    " digits each raised to the power of the number of digits.
    DATA(final) = REDUCE i(
      LET digits = extract_digits( num ) IN
      INIT sum = 0 FOR n IN digits
      NEXT sum = sum + iPow( base = n exp = lines( digits ) ) ).
    result = boolc( final = num ).
  ENDMETHOD.

  METHOD extract_digits.
    " determine all numeric digits in the given integer
    DATA(n) = number.
    WHILE n > 9.
      APPEND n MOD 10 TO digits.
      n = n DIV 10.
    ENDWHILE.
    APPEND n TO digits.
  ENDMETHOD.

ENDCLASS.
