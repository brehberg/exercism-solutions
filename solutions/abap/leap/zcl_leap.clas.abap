CLASS zcl_leap DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS leap
      IMPORTING
        year          TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
    DATA year TYPE i.
    METHODS is_divisible_by
      IMPORTING number        TYPE i
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_leap IMPLEMENTATION.

  METHOD leap.
    me->year = year.
    " Occurs on every year that is evenly divisible by 4
    " except every year that is evenly divisible by 100
    " unless the year is also evenly divisible by 400.
    result = COND #(
      WHEN is_divisible_by( 100 )
      THEN is_divisible_by( 400 )
      ELSE is_divisible_by( 4 ) ).
  ENDMETHOD.

  METHOD is_divisible_by.
    result = boolc( year MOD number = 0 ).
  ENDMETHOD.

ENDCLASS.