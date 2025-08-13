CLASS zcl_reverse_string DEFINITION PUBLIC.
  PUBLIC SECTION.
    METHODS reverse_string
      IMPORTING
        input         TYPE string
      RETURNING
        VALUE(result) TYPE string.
ENDCLASS.

CLASS zcl_reverse_string IMPLEMENTATION.

  METHOD reverse_string.
    " solution without using built-in function reverse()
    DATA offset TYPE i.
    offset = strlen( input )  - 1.
    WHILE offset >= 0.
      result = result && input+offset(1).
      offset = offset - 1.
    ENDWHILE.
  ENDMETHOD.

ENDCLASS.