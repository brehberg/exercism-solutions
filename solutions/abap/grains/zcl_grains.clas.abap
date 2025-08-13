CLASS zcl_grains DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES type_result TYPE p LENGTH 16 DECIMALS 0.
    METHODS square
      IMPORTING
        input         TYPE i
      RETURNING
        VALUE(result) TYPE type_result
      RAISING
        cx_parameter_invalid.
    METHODS total
      RETURNING
        VALUE(result) TYPE type_result
      RAISING
        cx_parameter_invalid.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_grains IMPLEMENTATION.

  METHOD square.
    " Square returns how many grains were on a given square
    result = COND #(
      WHEN input < 1 OR input > 64
      THEN THROW cx_parameter_invalid( )
      ELSE ipow( base = 2 exp = input - 1 ) ).
  ENDMETHOD.

  METHOD total.
    " Total returns the number of grains of wheat on a chess board
    " given that the number on each square doubles. (where square 1
    " has one grain, square 2 has two grains, and so on). There are
    " 64 total squares on a chess board.
    result = ipow( base = 2 exp = 64 ) - 1.
  ENDMETHOD.

ENDCLASS.