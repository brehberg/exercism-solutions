CLASS zcl_darts DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS score
      IMPORTING
        x             TYPE f
        y             TYPE f
      RETURNING
        VALUE(result) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_darts IMPLEMENTATION.

  METHOD score.
    result = COND #(
      LET dist = sqrt( x * x + y * y ) IN
      WHEN dist <= 1 THEN 10
      WHEN dist <= 5 THEN 5
      WHEN dist <= 10 THEN 1
      ELSE 0 ).
  ENDMETHOD.

ENDCLASS.