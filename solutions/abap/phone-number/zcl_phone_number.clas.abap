CLASS zcl_phone_number DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS clean
      IMPORTING
        !number       TYPE string
      RETURNING
        VALUE(result) TYPE string
      RAISING
        cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS extract_digits
      IMPORTING !number       TYPE string
      RETURNING VALUE(digits) TYPE string.

ENDCLASS.


CLASS zcl_phone_number IMPLEMENTATION.

  METHOD clean.
    result = extract_digits( number ).

    " all NANP-numbers share the same country code
    IF strlen( result ) = 11 AND result(1) = 1.
      result = result+1.
    ENDIF.

    " area and exchange codes only digits from 2 through 9
    IF strlen( result ) <> 10
        OR result+0(1) CA '01'  " area code
        OR result+3(1) CA '01'. " exchange code
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD extract_digits.
    digits = number.
    " determine all numeric characters in the given string
    REPLACE ALL OCCURRENCES OF REGEX '\D' IN digits WITH ``.
  ENDMETHOD.

ENDCLASS.