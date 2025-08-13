CLASS zcl_raindrops DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS raindrops
      IMPORTING
        input         TYPE i
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA sounds TYPE string_table.
    DATA number TYPE i.
    METHODS divisible_by
      IMPORTING divisor     TYPE i
      RETURNING VALUE(flag) TYPE abap_bool.

ENDCLASS.

CLASS zcl_raindrops IMPLEMENTATION.

  METHOD raindrops.
    " Returns a string that contains the raindrop sounds
    " corresponding to potential factors of given input.
    number = input.

    " The rules of raindrops are that if a given number:
    "   has 3 as a factor, add 'Pling' to the result.
    "   has 5 as a factor, add 'Plang' to the result.
    "   has 7 as a factor, add 'Plong' to the result.
    "   does not have any of 3, 5, or 7 as a factor,
    "   result should be the digits of the number.
    result = COND #(
      LET index = COND #( WHEN divisible_by( 3 ) THEN 1 )
                + COND #( WHEN divisible_by( 5 ) THEN 2 )
                + COND #( WHEN divisible_by( 7 ) THEN 4 )
      IN WHEN index = 0 THEN input ELSE sounds[ index ] ).
  ENDMETHOD.

  METHOD divisible_by.
    flag = boolc( number MOD divisor = 0 ).
  ENDMETHOD.

  METHOD class_constructor.
    sounds = VALUE #(
      ( `Pling` ) ( `Plang` ) ( `PlingPlang` )
      ( `Plong` ) ( `PlingPlong` )
      ( `PlangPlong` ) ( `PlingPlangPlong` ) ).
  ENDMETHOD.
ENDCLASS.