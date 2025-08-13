CLASS zcl_leap DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS leap
      IMPORTING
        year          TYPE i
      RETURNING
        VALUE(result) TYPE abap_bool.
  PRIVATE SECTION.
    DATA year TYPE i.
    METHODS is_leap_year
      RETURNING VALUE(result) TYPE abap_bool.      
    METHODS is_divisible_by
      IMPORTING number        TYPE i
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_leap IMPLEMENTATION.

  METHOD leap.
    me->year = year.
    result = is_leap_year( ).
  ENDMETHOD.

  METHOD is_leap_year.
    result = boolc( is_divisible_by( 4 ) = abap_true AND
              ( NOT is_divisible_by( 100 ) = abap_true
                 OR is_divisible_by( 400 ) = abap_true ) ).
  ENDMETHOD.

  METHOD is_divisible_by.
    result = boolc( year MOD number = 0 ).
  ENDMETHOD.

ENDCLASS.