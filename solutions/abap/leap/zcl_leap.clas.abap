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
      IMPORTING num           TYPE i
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.

CLASS zcl_leap IMPLEMENTATION.

  METHOD leap.
    me->year = year.
    " Occurs on every year that is evenly divisible by 4
    " except every year that is evenly divisible by 100
    " unless the year is also evenly divisible by 400.
    result = boolc( is_divisible_by( 4 )
                AND NOT is_divisible_by( 100 )
                OR is_divisible_by( 400 ) ).
  ENDMETHOD.

  METHOD is_divisible_by.
    result = boolc( year MOD num = 0 ).
  ENDMETHOD.

ENDCLASS.