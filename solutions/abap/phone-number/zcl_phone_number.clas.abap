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
    TYPES:
      digit  TYPE n LENGTH 1,
      digits TYPE STANDARD TABLE OF digit WITH EMPTY KEY.
    METHODS extract_digits
      IMPORTING !number       TYPE string
      RETURNING VALUE(digits) TYPE digits.
    METHODS allowed
      IMPORTING digit       TYPE digit
      RETURNING VALUE(flag) TYPE abap_bool.

ENDCLASS.


CLASS zcl_phone_number IMPLEMENTATION.

  METHOD clean.
    DATA(digits) = extract_digits( number ).

    " all NANP-numbers share the same country code
    IF lines( digits ) = 11 AND digits[ 1 ] = 1.
      DELETE digits INDEX 1.
    ENDIF.

    " area and exchange codes only digits from 2 through 9
    DATA allowed TYPE RANGE OF digit.
    allowed = VALUE #(
      ( sign = 'I' option = 'BT' low = 2 high = 9 ) ).
    IF lines( digits ) <> 10
        OR NOT allowed( digits[ 1 ] )
        OR NOT allowed( digits[ 4 ] ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    result = concat_lines_of( digits ).
  ENDMETHOD.

  METHOD allowed.
    flag = boolc( digit >= 2 AND digit <= 9 ).
  ENDMETHOD.

  METHOD extract_digits.
    " hard-coded local types to make FIND with REGEX work on Exercism
    TYPES: BEGIN OF submatch_result,
             offset TYPE i,
             length TYPE i,
           END OF submatch_result.
    TYPES submatch_result_tab TYPE STANDARD TABLE OF submatch_result WITH DEFAULT KEY.
    TYPES: BEGIN OF match_result,
             line       TYPE i,
             offset     TYPE i,
             length     TYPE i,
             submatches TYPE submatch_result_tab,
           END OF match_result.
    TYPES match_result_tab TYPE STANDARD TABLE OF match_result WITH DEFAULT KEY.

    " determine all numeric characters in the given string
    FIND ALL OCCURRENCES OF REGEX '\d' IN number
      RESULTS DATA(matches).
    digits = VALUE #( FOR match IN matches
      ( CONV #( number+match-offset(match-length) ) ) ).
  ENDMETHOD.

ENDCLASS.